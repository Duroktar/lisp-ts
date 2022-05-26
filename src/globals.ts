import assert from "assert";
import { FALSE, NIL, TRUE, UNDEF } from "./core/const";
import { Env } from "./core/env";
import { InvalidCallableExpression } from "./core/error";
import { evaluate } from "./core/eval";
import { expand } from "./core/expand";
import * as Lisp from "./core/lisp";
import { isNativeProc, isProc, NativeProc } from "./core/proc";
import { Sym } from "./core/sym";
import { SyntaxRulesDef } from "./core/syntax";
import type { List, Term } from "./core/terms";
import * as toString from "./core/toString";
import { loadFile } from "./load";
import { gcd, lcm } from "./math";
import * as Util from "./utils";

export function addGlobals(env: Env, l: Env, r: Env) {
  env.set('#t', TRUE);
  env.set('#f', FALSE);
  env.set('#<undef>', UNDEF);

  env.set('else', TRUE);
  env.set('otherwise', TRUE);
  env.set('nil', NIL);

  env.set('#cwd', process.cwd());

  function mkNativeProc(
    env: Env,
    name: string,
    params: string[],
    cb: (args: Term, env: Env) => any
  ): Term | NativeProc {

    const func = new class extends NativeProc {
      public name = name;
      public env = env;
      public params = params.map(Sym);
      public call = cb;
    };

    env.set(name, func);
    return func;
  }

  const callWithCC = ([proc]: any, env: Env) => {
    class RuntimeWarning extends Error { public retval?: any; }
    let ball = new RuntimeWarning("Sorry, can't continue this continuation any longer.");
    const throw_ = mkNativeProc(env, 'throw', ['retval'], ([retval]: any) => {
      ball.retval = retval; throw ball;
    });
    try {
      if (isNativeProc(proc)) {
        return proc.call([throw_ as Term], env);
      }
      throw new InvalidCallableExpression(proc);
    } catch (err) {
      if (err instanceof RuntimeWarning) {
        // console.log(`exiting call/cc [${id}] (THROWN)`)
        return ball.retval;
      }
      else {
        throw err;
      }
    }
  }

  mkNativeProc(env, 'list', ['args'], (args: any) => args);
  mkNativeProc(env, 'cons', ['a', 'b'], ([a, b]: any) => [a].concat(b));
  mkNativeProc(env, 'car', ['args'], (args: any) => Lisp.car(args));
  mkNativeProc(env, 'cdr', ['args'], (args: any) => Lisp.cdr(args));
  mkNativeProc(env, 'set-car!', ['l', 'v'], ([l, v]: any) => l[0] = v);
  mkNativeProc(env, 'set-cdr!', ['l', 'v'], ([l, v]: any) => l[1] = v);

  mkNativeProc(env, 'locals', [], (_, a) => { return a.keys().map(Sym); });
  mkNativeProc(env, 'env', [], () => { return env; });
  mkNativeProc(env, 'env->size', [], () => { return env.size(); });
  mkNativeProc(env, 'env->keys', [], () => { return env.keys(); });
  mkNativeProc(env, 'env->values', [], () => { return env.values(); });
  mkNativeProc(env, 'env->entries', [], () => { return env.entries(); });

  mkNativeProc(env, 'debugnf', ['name', 'x'], ([name, x]: any) => { console.log('[DEBUG-NF]:', toString.toString(name)); console.log(x); });
  mkNativeProc(env, 'debugn', ['name', 'x'], ([name, x]: any) => { console.log('[DEBUG-N]:', toString.toString(name)); console.log(x); return x; });
  mkNativeProc(env, 'debugf', ['x'], x => { console.log('[DEBUG-F]'); console.log(x); });
  mkNativeProc(env, 'debug', ['x'], x => { console.log('[DEBUG]'); console.log(x); return x; });
  mkNativeProc(env, 'printn', ['name', 'x'], ([name, x]: any) => { console.log(toString.toString(name), toString.toString(x)); });
  mkNativeProc(env, 'printr', ['x'], ([x]: any) => { toString.print(x); return x});
  mkNativeProc(env, 'prints', ['...xs'], ([...xs]: any) => { console.log(...xs); });
  mkNativeProc(env, 'print', ['...xs'], ([...xs]: any) => { console.log(...xs.map((x: any) => toString.toString(x))); });
  mkNativeProc(env, 'show', ['x'], x => { console.log(x); });
  mkNativeProc(env, 'inspect', ['x'], ([x]: any) => { return toString.toString(x, true); });
  mkNativeProc(env, 'break', ['args'], (args: any, env) => { debugger; return evaluate(args, env); });
  mkNativeProc(env, 'error', ['x', 'code?'], ([x, code = 1]: any) => { console.error(x); process.exit(code); });
  mkNativeProc(env, 'debug-macro!', ['val'], ([val]: any) => { SyntaxRulesDef.debug = !Util.isF(val) });

  mkNativeProc(env, 'gensym', [], () => Symbol());

  mkNativeProc(env, 'pair?', ['obj'], ([obj]: any) => Util.toL(Util.isPair(obj)));
  mkNativeProc(env, 'eqv?', ['a', 'b'], ([a, b]: any) => Util.toL(Util.isEq(a, b)));
  mkNativeProc(env, 'equal?', ['a', 'b'], ([a, b]: any) => Util.toL(toString.toString(a) === toString.toString(b)));

  mkNativeProc(env, 'assv', ['x', 'lst'], ([x, lst]: any) => {

    return Sym('#f')
  });

  mkNativeProc(env, 'length', ['list'], ([list]: any) => Util.isList(list) && list.length);
  mkNativeProc(env, 'reverse', ['list'], ([list]: any) => Util.isList(list) && [...list].reverse());

  mkNativeProc(env, 'min', ['args'], (args: any) => args.reduce((acc: any, val: any) => Math.min(acc, val)));
  mkNativeProc(env, 'max', ['args'], (args: any) => args.reduce((acc: any, val: any) => Math.max(acc, val)));
  mkNativeProc(env, 'abs', ['n'], ([n]: any) => Math.abs(n));
  mkNativeProc(env, 'quotient', ['x', 'y'], ([x, y]: any) => x/y|0);
  mkNativeProc(env, 'gcd', ['a', 'b'], ([a, b]: any) => { return gcd(a, b) });
  mkNativeProc(env, 'lcm', ['a', 'b'], ([a, b]: any) => { return lcm(a, b) });
  mkNativeProc(env, 'floor', ['n'], ([n]: any) => Math.floor(n));
  mkNativeProc(env, 'ceiling', ['n'], ([n]: any) => Math.ceil(n));
  mkNativeProc(env, 'truncate', ['n'], ([n]: any) => Math.trunc(n));
  mkNativeProc(env, 'round', ['n'], ([n]: any) => Math.round(n));
  mkNativeProc(env, 'exp', ['n'], ([n]: any) => Math.exp(n));
  mkNativeProc(env, 'log', ['n'], ([n]: any) => Math.log(n));
  mkNativeProc(env, 'sin', ['n'], ([n]: any) => Math.sin(n));
  mkNativeProc(env, 'cos', ['n'], ([n]: any) => Math.cos(n));
  mkNativeProc(env, 'tan', ['n'], ([n]: any) => Math.tan(n));
  mkNativeProc(env, 'asin', ['n'], ([n]: any) => Math.asin(n));
  mkNativeProc(env, 'acos', ['n'], ([n]: any) => Math.acos(n));
  mkNativeProc(env, 'atan', ['y', 'x'], ([y, x]: any) => Util.isNone(x) ? Math.atan(y) : Math.atan2(y, x));
  mkNativeProc(env, 'sqrt', ['n'], ([n]: any) => Math.sqrt(n));
  mkNativeProc(env, 'expt', ['n'], ([n]: any) => Math.exp(n));

  mkNativeProc(env, '+', ['args'], (args: any) => args.reduce((acc: any, val: any) => acc + val, 0));
  mkNativeProc(env, '*', ['args'], (args: any) => args.reduce((acc: any, val: any) => acc * val, 1));
  mkNativeProc(env, '-', ['args'], (args: any) => {
    Util.assert(args.length > 0, "procedure requires at least one argument: (-)")
    if (args.length === 1) return -args[0]
    else return args.reduce((acc: any, val: any) => acc - val)
  });
  mkNativeProc(env, '/', ['args'], (args: any) => {
    Util.assert(args.length > 0, "procedure requires at least one argument: (/)")
    args.reduce((acc: any, val: any) => acc / val)
  });
  mkNativeProc(env, '=', ['args'], (args: any) => args.reduce((acc: any, val: any) => Util.toL(acc === val)));
  mkNativeProc(env, '>', ['args'], (args: any) => args.reduce((acc: any, val: any) => Util.toL(acc > val)));
  mkNativeProc(env, '<', ['args'], (args: any) => args.reduce((acc: any, val: any) => Util.toL(acc < val)));
  mkNativeProc(env, '>=', ['args'], ([l, r]: any) => Util.toL(l >= r));
  mkNativeProc(env, '<=', ['args'], ([l, r]: any) => Util.toL(l <= r));

  mkNativeProc(env, 'zero?', ['n'], ([n]: any) => Util.toL(n === 0));
  mkNativeProc(env, 'number?', ['n'], ([n]: any) => Util.toL(Util.isNum(n)));
  mkNativeProc(env, 'positive?', ['n'], ([n]: any) => Util.toL(Util.isNum(n) && n > 0));
  mkNativeProc(env, 'negative?', ['n'], ([n]: any) => Util.toL(Util.isNum(n) && n < 0));
  mkNativeProc(env, 'odd?', ['n'], ([n]: any) => Util.toL(Util.isNum(n) && n % 2 !== 0));
  mkNativeProc(env, 'even?', ['n'], ([n]: any) => Util.toL(Util.isNum(n) && n % 2 === 0));
  mkNativeProc(env, 'null?', ['n'], ([n]: any) => Util.toL(Util.isEmpty(n)));
  mkNativeProc(env, 'list?', ['n'], ([n]: any) => Util.toL(Util.isList(n)));
  mkNativeProc(env, 'symbol?', ['n'], ([n]: any) => Util.toL(Util.isSym(n)));
  mkNativeProc(env, 'symbol->string', ['n'], ([n]: any) => {
    Util.assert(Util.isSym(n), `"symbol->string" procedure takes a 'symbol' as an argument`);
    return toString.toString(n)
  });
  mkNativeProc(env, 'number->string', ['n', 'radix?'], ([n, radix]: any) => {
    Util.assert(Util.isNum(n), `"number->string" procedure takes a 'number' as an argument`);
    return (<number>n).toString(radix ?? 10);
  });
  mkNativeProc(env, 'string->number', ['n', 'radix?'], ([n, radix]: any) => {
    Util.assert(Util.isString(n), `"string->number" procedure takes a 'string' as an argument`);
    return parseInt(n, radix ?? 10);
  });
  mkNativeProc(env, 'string->symbol', ['n'], ([n]: any) => {
    Util.assert(Util.isString(n), `"string->symbol" procedure takes a 'string' as an argument`);
    return Sym(n)
  });
  mkNativeProc(env, 'string?', ['n'], ([n]: any) => Util.toL(Util.isString(n)));
  mkNativeProc(env, 'string-length', ['n'], ([n]: any) => {
    Util.assert(Util.isString(n))
    return n.length
  });
  mkNativeProc(env, 'string-ref', ['string', 'k'], ([string, k]: any) => {
    Util.assert(Util.isString(string) && string.length >= k)
    return string[k]
  });
  mkNativeProc(env, 'string-set', ['string', 'k', 'char'], ([string, k, char]: any) => {
    Util.assert(Util.isString(string) && string.length >= k)
    Util.assert(Util.isChar(char))
    string[k] = char
    return UNDEF
  });
  mkNativeProc(env, 'string=?', ['string1', 'string2'], ([string1, string2]: any) => {
    Util.assert(Util.isString(string1) && Util.isString(string2))
    return Util.toL(string1 === string2)
  });
  mkNativeProc(env, 'string-ci=?', ['string1', 'string2'], ([string1, string2]: any) => {
    Util.assert(Util.isString(string1) && Util.isString(string2))
    if (string1.length !== string2.length)
      return Util.toL(false)
    for (let i = 0; i < string1.length; i++) {
      if ((<string>string1[i]).toLowerCase() === string2[i].toLowerCase())
        continue
      return Util.toL(false)
    }
    return Util.toL(true)
  });
  mkNativeProc(env, 'string<?', ['string1', 'string2'], ([string1, string2]: any) => {
    Util.assert(Util.isString(string1) && Util.isString(string2))
    return Util.toL(string1 < string2)
  });
  mkNativeProc(env, 'string>?', ['string1', 'string2'], ([string1, string2]: any) => {
    Util.assert(Util.isString(string1) && Util.isString(string2))
    return Util.toL(string1 > string2)
  });
  mkNativeProc(env, 'string<=?', ['string1', 'string2'], ([string1, string2]: any) => {
    Util.assert(Util.isString(string1) && Util.isString(string2))
    return Util.toL(string1 <= string2)
  });
  mkNativeProc(env, 'string>=?', ['string1', 'string2'], ([string1, string2]: any) => {
    Util.assert(Util.isString(string1) && Util.isString(string2))
    return Util.toL(string1 >= string2)
  });
  mkNativeProc(env, 'string-ci<?', ['string1', 'string2'], ([string1, string2]: any) => {
    Util.assert(Util.isString(string1) && Util.isString(string2))
    return Util.toL(string1.toLowerCase() < string2.toLowerCase())
  });
  mkNativeProc(env, 'string-ci>?', ['string1', 'string2'], ([string1, string2]: any) => {
    Util.assert(Util.isString(string1) && Util.isString(string2))
    return Util.toL(string1.toLowerCase() > string2.toLowerCase())
  });
  mkNativeProc(env, 'string-ci<=?', ['string1', 'string2'], ([string1, string2]: any) => {
    Util.assert(Util.isString(string1) && Util.isString(string2))
    return Util.toL(string1.toLowerCase() <= string2.toLowerCase())
  });
  mkNativeProc(env, 'string-ci>=?', ['string1', 'string2'], ([string1, string2]: any) => {
    Util.assert(Util.isString(string1) && Util.isString(string2))
    return Util.toL(string1.toLowerCase() >= string2.toLowerCase())
  });
  mkNativeProc(env, 'substring', ['string', 'start', 'end'], ([string, start, end]: any) => {
    // Util.assert(Util.isString(string) && string.length >= start && string.length >= end && start < end)
    return (<string>string).slice(start, end)
  });
  mkNativeProc(env, 'string-append', ['string', '...xs'], ([string, ...xs]: any) => {
    // Util.assert(Util.isString(string))
    return (<string>string).concat(...xs)
  });
  mkNativeProc(env, 'string->list', ['string', '...'], ([string]: any) => {
    Util.assert(Util.isString(string))
    return (<string>string).split('')
  });
  mkNativeProc(env, 'list->string', ['list', '...'], ([list]: any) => {
    Util.assert(Util.isList(list) && list.map(o => Util.isString(o)))
    return (<List>list).join('')
  });
  mkNativeProc(env, 'string-copy', ['string'], ([string]: any) => {
    Util.assert(Util.isString(string))
    return String(string)
  });
  mkNativeProc(env, 'string-fill', ['string', 'char'], ([string, char]: any) => {
    Util.assert(Util.isString(string))
    Util.assert(Util.isChar(char))
    string.replaceAll(/.*/, char)
    return string
  });
  mkNativeProc(env, 'string-pad-end', ['string', 'maxLength', '.', 'fillString'], ([string, maxLength, ...[fillString]]: any) => {
    Util.assert(Util.isString(string));
    Util.assert(Util.isNum(maxLength));
    return (string as string).padEnd(maxLength, fillString);
  });
  mkNativeProc(env, 'string-pad-start', ['string', 'maxLength', '.', 'fillString'], ([string, maxLength, ...[fillString]]: any) => {
    Util.assert(Util.isString(string));
    Util.assert(Util.isNum(maxLength));
    return (string as string).padStart(maxLength, fillString);
  });
  mkNativeProc(env, 'procedure?', ['obj'], ([obj]: any) => {
    return Util.toL(isProc(obj))
  });

  mkNativeProc(env, 'set-macro-character', ['char', 'cb'], ([char, cb]: any, env) => {
    l.setFrom(char, (locals: any) => {
      const proc = evaluate(cb, env);
      if (isProc(proc)) {
        mkNativeProc(proc.env, 'read', ['read'], ([locals]: any) => locals.parse());
        mkNativeProc(proc.env, 'advance', ['advance'], ([locals]: any) => locals.advance());
        mkNativeProc(proc.env, 'current', ['current'], ([locals]: any) => locals.current());
        mkNativeProc(proc.env, 'isEOF', ['isEOF'], ([locals]: any) => locals.isEOF());
        mkNativeProc(proc.env, 'isSpace', ['isSpace'], ([locals]: any) => locals.isSpace());
        mkNativeProc(proc.env, 'isNewLine', ['isNewLine'], ([locals]: any) => locals.isNewLine());
        return evaluate([proc, locals, toString.toString(char)], env);
      }
      throw new Error('Nope @ set-macro-character');
    });
  });

  mkNativeProc(env, 'call/cc', ['throw'], callWithCC);
  mkNativeProc(env, 'call-with-current-continuation', ['throw'], callWithCC);

  mkNativeProc(env, 'try', ['callable'], ([callable]: any) => {
    try {
      if (isNativeProc(callable)) {
        return [TRUE, callable.call([], env)];
      }
      return [FALSE, ['InvalidCallableExpression']]
    } catch (err) {
      if (err instanceof Error)
        return [FALSE, [err.message]];
      if (typeof err === 'string')
        return [FALSE, [err]];
      return [FALSE, ['UnknownError']];
    }
  });

  mkNativeProc(env, 'macroexpand', ['expr'], ([args]: any) => expand(args, true, env));

  const importableNameIndex = Util.searchIdx('stdlib', 'tests', 'samples')

  const parseLoadSymbol = (sym: symbol, ext = '.scm') => {
    const repr = sym.description
    assert(repr, 'A symbol has no name.. - Some Guy')
    assert(
      repr.includes('/') &&
      repr.split('/')[0] in importableNameIndex,

      `Must import from a known namespace (eg: ${Object.keys(importableNameIndex).slice(0, 2).join(', ')}, etc.)`
    )
    return repr + ext
  }

  mkNativeProc(env, 'load', ['file'], ([file]: any) => {
    if (Util.isSym(file)) {
      return loadFile(parseLoadSymbol(file), env, l, r)
    }
    return loadFile(file, env, l, r)
  });

  mkNativeProc(env, 'reload', ['file'], ([file]: any) => {
    if (Util.isSym(file)) {
      return loadFile(parseLoadSymbol(file), env, l, r, true)
    }
    return loadFile(file, env, l, r, true)
  });
}
