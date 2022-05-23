import assert from "assert";
import { basename, dirname, join, isAbsolute, relative } from "path";
import { FALSE, TRUE, UNDEF } from "./core/const";
import { callWithCC } from "./core/cont";
import { Env } from "./core/env";
import { evaluate } from "./core/eval";
import { expand } from "./core/expand";
import * as Lisp from "./core/lisp";
import { readMacroTable } from "./core/macro";
import { Sym } from "./core/sym";
import { Atom, List } from "./core/terms";
import { executeFile, loadFile } from "./load";
import { gcd, lcm } from "./math";
import * as Util from "./utils";

export const env = new Env();

env.set('#t', TRUE);
env.set('#f', FALSE);
env.set('#<undef>', UNDEF);

env.set('else', TRUE);
env.set('otherwise', TRUE);

env.set('#cwd', process.cwd());

const { mkNativeFunc } = Util

mkNativeFunc(env, 'cons', ['a', 'b'], ([a, b]: any) => [a].concat(b));
mkNativeFunc(env, 'car', ['args'], (args: any) => Lisp.car(args));
mkNativeFunc(env, 'cdr', ['args'], (args: any) => Lisp.cdr(args));
mkNativeFunc(env, 'set-cdr!', ['l', 'v'], ([l, v]: any) => l[1] = v);

mkNativeFunc(env, 'locals', [], (_, a) => { return a.keys().map(Sym); });
mkNativeFunc(env, 'env', [], () => { return env; });
mkNativeFunc(env, 'env->size', [], () => { return env.size(); });
mkNativeFunc(env, 'env->keys', [], () => { return env.keys(); });
mkNativeFunc(env, 'env->values', [], () => { return env.values(); });
mkNativeFunc(env, 'env->entries', [], () => { return env.entries(); });

mkNativeFunc(env, 'debugnf', ['name', 'x'], ([name, x]: any) => { console.log('[DEBUG-NF]:', Util.toString(name)); console.log(x); });
mkNativeFunc(env, 'debugn', ['name', 'x'], ([name, x]: any) => { console.log('[DEBUG-N]:', Util.toString(name)); console.log(x); return x; });
mkNativeFunc(env, 'debugf', ['x'], x => { console.log('[DEBUG-F]'); console.log(x); });
mkNativeFunc(env, 'debug', ['x'], x => { console.log('[DEBUG]'); console.log(x); return x; });
mkNativeFunc(env, 'printn', ['name', 'x'], ([name, x]: any) => { console.log(Util.toString(name), Util.toString(x)); });
mkNativeFunc(env, 'printr', ['x'], ([x]: any) => { Util.print(x); return x});
mkNativeFunc(env, 'prints', ['...xs'], ([...xs]: any) => { console.log(...xs); });
mkNativeFunc(env, 'print', ['...xs'], ([...xs]: any) => { console.log(...xs.map((x: any) => Util.toString(x))); });
mkNativeFunc(env, 'show', ['x'], x => { console.log(x); });
mkNativeFunc(env, 'display', ['x'], ([x]: any) => { Util.print(x, false, 'lambda'); });
mkNativeFunc(env, 'newline', [], () => { console.log(); });
mkNativeFunc(env, 'inspect', ['x'], ([x]: any) => { return Util.toString(x, true); });
mkNativeFunc(env, 'break', ['args'], (args: any, env) => { debugger; return evaluate(args, env); });
mkNativeFunc(env, 'error', ['x', 'code?'], ([x, code = 1]: any) => { console.error(x); process.exit(code); });

mkNativeFunc(env, 'gensym', [], () => Symbol());

mkNativeFunc(env, 'pair?', ['obj'], ([obj]: any) => Util.toL(Util.isPair(obj)));
mkNativeFunc(env, 'eq?', ['a', 'b'], ([a, b]: any) => Util.toL(Util.isEq(a, b)));
mkNativeFunc(env, 'eqv?', ['a', 'b'], ([a, b]: any) => Util.toL(Util.isEq(a, b)));
mkNativeFunc(env, 'equal?', ['a', 'b'], ([a, b]: any) => {
  return Util.toL(Util.toString(a) === Util.toString(b))
});

mkNativeFunc(env, 'assv', ['x', 'lst'], ([x, lst]: any) => {

  return Sym('#f')
});

mkNativeFunc(env, 'length', ['list'], ([list]: any) => Util.isList(list) && list.length);
mkNativeFunc(env, 'reverse', ['list'], ([list]: any) => Util.isList(list) && [...list].reverse());

mkNativeFunc(env, 'min', ['args'], (args: any) => args.reduce((acc: any, val: any) => Math.min(acc, val)));
mkNativeFunc(env, 'max', ['args'], (args: any) => args.reduce((acc: any, val: any) => Math.max(acc, val)));
mkNativeFunc(env, 'abs', ['n'], ([n]: any) => Math.abs(n));
mkNativeFunc(env, 'quotient', ['x', 'y'], ([x, y]: any) => x/y|0);
mkNativeFunc(env, 'gcd', ['a', 'b'], ([a, b]: any) => { return gcd(a, b) });
mkNativeFunc(env, 'lcm', ['a', 'b'], ([a, b]: any) => { return lcm(a, b) });
mkNativeFunc(env, 'floor', ['n'], ([n]: any) => Math.floor(n));
mkNativeFunc(env, 'ceiling', ['n'], ([n]: any) => Math.ceil(n));
mkNativeFunc(env, 'truncate', ['n'], ([n]: any) => Math.trunc(n));
mkNativeFunc(env, 'round', ['n'], ([n]: any) => Math.round(n));
mkNativeFunc(env, 'exp', ['n'], ([n]: any) => Math.exp(n));
mkNativeFunc(env, 'log', ['n'], ([n]: any) => Math.log(n));
mkNativeFunc(env, 'sin', ['n'], ([n]: any) => Math.sin(n));
mkNativeFunc(env, 'cos', ['n'], ([n]: any) => Math.cos(n));
mkNativeFunc(env, 'tan', ['n'], ([n]: any) => Math.tan(n));
mkNativeFunc(env, 'asin', ['n'], ([n]: any) => Math.asin(n));
mkNativeFunc(env, 'acos', ['n'], ([n]: any) => Math.acos(n));
mkNativeFunc(env, 'atan', ['y', 'x'], ([y, x]: any) => Util.isNone(x) ? Math.atan(y) : Math.atan2(y, x));
mkNativeFunc(env, 'sqrt', ['n'], ([n]: any) => Math.sqrt(n));
mkNativeFunc(env, 'expt', ['n'], ([n]: any) => Math.exp(n));

mkNativeFunc(env, '+', ['args'], (args: any) => args.reduce((acc: any, val: any) => acc + val, 0));
mkNativeFunc(env, '*', ['args'], (args: any) => args.reduce((acc: any, val: any) => acc * val, 1));
mkNativeFunc(env, '-', ['args'], (args: any) => {
  Util.assert(args.length > 0, "procedure requires at least one argument: (-)")
  if (args.length === 1) return -args[0]
  else return args.reduce((acc: any, val: any) => acc - val)
});
mkNativeFunc(env, '/', ['args'], (args: any) => {
  Util.assert(args.length > 0, "procedure requires at least one argument: (/)")
  args.reduce((acc: any, val: any) => acc / val)
});
mkNativeFunc(env, '=', ['args'], (args: any) => args.reduce((acc: any, val: any) => Util.toL(acc === val)));
mkNativeFunc(env, '>', ['args'], (args: any) => args.reduce((acc: any, val: any) => Util.toL(acc > val)));
mkNativeFunc(env, '<', ['args'], (args: any) => args.reduce((acc: any, val: any) => Util.toL(acc < val)));
mkNativeFunc(env, '>=', ['args'], ([l, r]: any) => Util.toL(l >= r));
mkNativeFunc(env, '<=', ['args'], ([l, r]: any) => Util.toL(l <= r));

mkNativeFunc(env, 'zero?', ['n'], ([n]: any) => Util.toL(n === 0));
mkNativeFunc(env, 'number?', ['n'], ([n]: any) => Util.toL(Util.isNum(n)));
mkNativeFunc(env, 'positive?', ['n'], ([n]: any) => Util.toL(Util.isNum(n) && n > 0));
mkNativeFunc(env, 'negative?', ['n'], ([n]: any) => Util.toL(Util.isNum(n) && n < 0));
mkNativeFunc(env, 'odd?', ['n'], ([n]: any) => Util.toL(Util.isNum(n) && n % 2 !== 0));
mkNativeFunc(env, 'even?', ['n'], ([n]: any) => Util.toL(Util.isNum(n) && n % 2 === 0));
mkNativeFunc(env, 'boolean?', ['n'], ([n]: any) => Util.toL(n === FALSE || n === TRUE));
mkNativeFunc(env, 'null?', ['n'], ([n]: any) => Util.toL(Util.isEmpty(n)));
mkNativeFunc(env, 'list?', ['n'], ([n]: any) => Util.toL(Util.isList(n)));
mkNativeFunc(env, 'symbol?', ['n'], ([n]: any) => Util.toL(Util.isSym(n)));
mkNativeFunc(env, 'symbol->string', ['n'], ([n]: any) => {
  Util.assert(Util.isSym(n), `"symbol->string" procedure takes a 'symbol' as an argument`);
  return Util.toString(n)
});
mkNativeFunc(env, 'number->string', ['n', 'radix?'], ([n, radix]: any) => {
  Util.assert(Util.isNum(n), `"number->string" procedure takes a 'number' as an argument`);
  return (<number>n).toString(radix ?? 10);
});
mkNativeFunc(env, 'string->number', ['n', 'radix?'], ([n, radix]: any) => {
  Util.assert(Util.isString(n), `"string->number" procedure takes a 'string' as an argument`);
  return parseInt(n, radix ?? 10);
});
mkNativeFunc(env, 'string->symbol', ['n'], ([n]: any) => {
  Util.assert(Util.isString(n), `"string->symbol" procedure takes a 'string' as an argument`);
  return Sym(n)
});
mkNativeFunc(env, 'string?', ['n'], ([n]: any) => Util.toL(Util.isString(n)));
mkNativeFunc(env, 'string-length', ['n'], ([n]: any) => {
  Util.assert(Util.isString(n))
  return n.length
});
mkNativeFunc(env, 'string-ref', ['string', 'k'], ([string, k]: any) => {
  Util.assert(Util.isString(string) && string.length >= k)
  return string[k]
});
mkNativeFunc(env, 'string-set', ['string', 'k', 'char'], ([string, k, char]: any) => {
  Util.assert(Util.isString(string) && string.length >= k)
  Util.assert(Util.isChar(char))
  string[k] = char
  return UNDEF
});
mkNativeFunc(env, 'string=?', ['string1', 'string2'], ([string1, string2]: any) => {
  Util.assert(Util.isString(string1) && Util.isString(string2))
  return Util.toL(string1 === string2)
});
mkNativeFunc(env, 'string-ci=?', ['string1', 'string2'], ([string1, string2]: any) => {
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
mkNativeFunc(env, 'string<?', ['string1', 'string2'], ([string1, string2]: any) => {
  Util.assert(Util.isString(string1) && Util.isString(string2))
  return Util.toL(string1 < string2)
});
mkNativeFunc(env, 'string>?', ['string1', 'string2'], ([string1, string2]: any) => {
  Util.assert(Util.isString(string1) && Util.isString(string2))
  return Util.toL(string1 > string2)
});
mkNativeFunc(env, 'string<=?', ['string1', 'string2'], ([string1, string2]: any) => {
  Util.assert(Util.isString(string1) && Util.isString(string2))
  return Util.toL(string1 <= string2)
});
mkNativeFunc(env, 'string>=?', ['string1', 'string2'], ([string1, string2]: any) => {
  Util.assert(Util.isString(string1) && Util.isString(string2))
  return Util.toL(string1 >= string2)
});
mkNativeFunc(env, 'string-ci<?', ['string1', 'string2'], ([string1, string2]: any) => {
  Util.assert(Util.isString(string1) && Util.isString(string2))
  return Util.toL(string1.toLowerCase() < string2.toLowerCase())
});
mkNativeFunc(env, 'string-ci>?', ['string1', 'string2'], ([string1, string2]: any) => {
  Util.assert(Util.isString(string1) && Util.isString(string2))
  return Util.toL(string1.toLowerCase() > string2.toLowerCase())
});
mkNativeFunc(env, 'string-ci<=?', ['string1', 'string2'], ([string1, string2]: any) => {
  Util.assert(Util.isString(string1) && Util.isString(string2))
  return Util.toL(string1.toLowerCase() <= string2.toLowerCase())
});
mkNativeFunc(env, 'string-ci>=?', ['string1', 'string2'], ([string1, string2]: any) => {
  Util.assert(Util.isString(string1) && Util.isString(string2))
  return Util.toL(string1.toLowerCase() >= string2.toLowerCase())
});
mkNativeFunc(env, 'substring', ['string', 'start', 'end'], ([string, start, end]: any) => {
  // Util.assert(Util.isString(string) && string.length >= start && string.length >= end && start < end)
  return (<string>string).slice(start, end)
});
mkNativeFunc(env, 'string-append', ['string', '...xs'], ([string, ...xs]: any) => {
  // Util.assert(Util.isString(string))
  return (<string>string).concat(...xs)
});
mkNativeFunc(env, 'string->list', ['string', '...'], ([string]: any) => {
  Util.assert(Util.isString(string))
  return (<string>string).split('')
});
mkNativeFunc(env, 'list->string', ['list', '...'], ([list]: any) => {
  Util.assert(Util.isList(list) && list.map(o => Util.isString(o)))
  return (<List>list).join('')
});
mkNativeFunc(env, 'string-copy', ['string'], ([string]: any) => {
  Util.assert(Util.isString(string))
  return String(string)
});
mkNativeFunc(env, 'string-fill', ['string', 'char'], ([string, char]: any) => {
  Util.assert(Util.isString(string))
  Util.assert(Util.isChar(char))
  string.replaceAll(/.*/, char)
  return string
});
mkNativeFunc(env, 'string-pad-end', ['string', 'maxLength', '.', 'fillString'], ([string, maxLength, ...[fillString]]: any) => {
  Util.assert(Util.isString(string));
  Util.assert(Util.isNum(maxLength));
  return (string as string).padEnd(maxLength, fillString);
});
mkNativeFunc(env, 'string-pad-start', ['string', 'maxLength', '.', 'fillString'], ([string, maxLength, ...[fillString]]: any) => {
  Util.assert(Util.isString(string));
  Util.assert(Util.isNum(maxLength));
  return (string as string).padStart(maxLength, fillString);
});
mkNativeFunc(env, 'procedure?', ['obj'], ([obj]: any) => {
  return Util.toL(Util.isCallable(obj))
});

mkNativeFunc(env, 'not', ['n'], ([n]: any) => Util.toL(n === FALSE));

mkNativeFunc(env, 'set-macro-character', ['char', 'cb'], ([char, cb]: any, env) => {
  readMacroTable[Util.toString(char)] = locals => {
    const proc = evaluate(cb, env);
    if (Util.isCallable(proc)) {
      mkNativeFunc(proc.env, 'read', ['read'], ([locals]: any) => locals.parse());
      mkNativeFunc(proc.env, 'advance', ['advance'], ([locals]: any) => locals.advance());
      mkNativeFunc(proc.env, 'current', ['current'], ([locals]: any) => locals.current());
      mkNativeFunc(proc.env, 'isEOF', ['isEOF'], ([locals]: any) => locals.isEOF());
      mkNativeFunc(proc.env, 'isSpace', ['isSpace'], ([locals]: any) => locals.isSpace());
      mkNativeFunc(proc.env, 'isNewLine', ['isNewLine'], ([locals]: any) => locals.isNewLine());
      return evaluate([proc, locals, Util.toString(char)], env);
    }
    throw new Error('Nope @ set-macro-character');
  };
});

mkNativeFunc(env, 'call/cc', ['throw'], callWithCC);
mkNativeFunc(env, 'call-with-current-continuation', ['throw'], callWithCC);

mkNativeFunc(env, 'try', ['callable'], ([callable]: any) => {
  try {
    if (Util.isCallable(callable)) {
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

mkNativeFunc(env, 'macroexpand', ['expr'], ([args]: any) => expand(args, true, env));

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

mkNativeFunc(env, 'load', ['file'], ([file]: any) => {
  if (Util.isSym(file)) {
    return loadFile(parseLoadSymbol(file))
  }
  return loadFile(file)
});
mkNativeFunc(env, 'reload', ['file'], ([file]: any) => {
  if (Util.isSym(file)) {
    return loadFile(parseLoadSymbol(file), true)
  }
  return loadFile(file, true)
});

// // Implement putchar/getchar to the terminal (from: https://github.com/udem-dlteam/ribbit)

// const putchar = (c: number) => {
//   let buffer = Buffer.alloc(1);
//   buffer[0] = c;
//   node_fs.writeSync(1, buffer, 0, 1);
//   return c;
// };

// const getchar_sync = () => {
//   let buffer = Buffer.alloc(1);
//   if (node_fs.readSync(0, buffer, 0, 1, 0))
//     return buffer[0];
//   return -1;
// };

/*
*
*  reader macros
*
*/
