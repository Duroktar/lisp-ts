import assert from "assert";
import { FALSE, NIL, TRUE, UNDEF } from "./core/const";
import { Resume } from "./core/cont";
import { Env } from "./core/env";
import { InvalidCallableExpression, MissingParenthesisError, NotImplementedError, UndefinedVariableError } from "./core/error";
import { evaluate } from "./core/eval";
import { expand } from "./core/expand";
import * as Lisp from "./core/lisp";
import { currentInputPort, currentOutputPort, InPort, isEofString, isInputPort, isOutputPort, OutPort } from "./core/port";
import { isNativeProc, isProc, NativeProc } from "./core/proc";
import { read } from "./core/read";
import { Sym } from "./core/sym";
import { SyntaxRulesDef } from "./core/syntax";
import type { List, Term } from "./core/terms";
import { toString, toStringSafe, print } from "./core/toString";
import { Vector } from "./core/vec";
import { Environment } from "./env";
import { loadFile } from "./load";
import { gcd, lcm } from "./math";
import * as Util from "./utils";

export function addGlobals(global: Environment) {
  const {env, readerEnv, lexicalEnv} = global

  env.set('#t', TRUE);
  env.set('#f', FALSE);
  env.set('#<undef>', UNDEF);

  env.set('else', TRUE);
  env.set('otherwise', TRUE);
  env.set('nil', NIL);

  env.set('#cwd', process.cwd());

  env.set('*default-input-port*', <any>InPort.fromStdIn())
  env.set('*current-input-port*', env.get('*default-input-port*'))

  env.set('*default-output-port*', <any>OutPort.fromStdOut())
  env.set('*current-output-port*', env.get('*default-output-port*'))

  env.set('*default-repl-prompt*', '> ')
  env.set('*current-repl-prompt*', env.get('*default-repl-prompt*'))

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

  mkNativeProc(env, 'locals', [], (_, a) => { return a.keys().sort().map(Sym); });
  mkNativeProc(env, 'env', [], () => { return env; });
  mkNativeProc(env, 'env->size', [], () => { return env.size(); });
  mkNativeProc(env, 'env->keys', [], () => { return env.keys().map(Sym); });
  mkNativeProc(env, 'env->values', [], () => { return env.values(); });
  mkNativeProc(env, 'env->entries', [], () => { return env.entries(); });

  mkNativeProc(env, 'debugnf', ['name', 'x'], ([name, x]: any) => { console.log('[DEBUG-NF]:', toString(name)); console.log(x); });
  mkNativeProc(env, 'debugn', ['name', 'x'], ([name, x]: any) => { console.log('[DEBUG-N]:', toString(name)); console.log(x); return x; });
  mkNativeProc(env, 'debugf', ['x'], x => { console.log('[DEBUG-F]'); console.log(x); });
  mkNativeProc(env, 'debug', ['x'], x => { console.log('[DEBUG]'); console.log(x); return x; });
  mkNativeProc(env, 'printn', ['name', 'x'], ([name, x]: any) => { console.log(toString(name), toString(x)); });
  mkNativeProc(env, 'printr', ['x'], ([x]: any) => { print(x); return x});
  mkNativeProc(env, 'prints', ['...xs'], ([...xs]: any) => { console.log(...xs); });
  mkNativeProc(env, 'print', ['...xs'], ([...xs]: any) => { console.log(...xs.map((x: any) => toString(x))); });
  mkNativeProc(env, 'show', ['x'], x => { console.log(x); });
  mkNativeProc(env, 'inspect', ['x'], ([x]: any) => { return toString(x, true); });
  mkNativeProc(env, 'break', ['args'], async (args: any, env) => { debugger; return await evaluate(args, env); });
  mkNativeProc(env, 'error', ['x', 'code?'], ([x, code = 1]: any) => { console.error(x); process.exit(code); });
  mkNativeProc(env, 'debug-macro!', ['val'], ([val]: any) => { SyntaxRulesDef.debug = !Util.isF(val) });

  mkNativeProc(env, 'gensym', [], () => Symbol());

  mkNativeProc(env, 'pair?', ['obj'], ([obj]: any) => Util.toL(Util.isPair(obj)));
  mkNativeProc(env, 'eqv?', ['a', 'b'], ([a, b]: any) => Util.toL(Util.isEq(a, b)));
  mkNativeProc(env, 'equal?', ['a', 'b'], ([a, b]: any) => Util.toL(toString(a) === toString(b)));

  // mkNativeProc(env, 'assv', ['x', 'lst'], ([x, lst]: any) => {

  //   return Sym('#f')
  // });

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
    return toString(n)
  });
  mkNativeProc(env, 'number->string', ['n', 'radix?'], ([n, radix]: any) => {
    Util.assert(Util.isNum(n), `"number->string" procedure takes a 'number' as an argument`);
    return (<number>n).toString(radix ?? 10);
  });


  mkNativeProc(env, 'procedure?', ['obj'], ([obj]: any) => {
    return Util.toL(isProc(obj))
  });

  mkNativeProc(env, 'set-macro-character', ['char', 'cb'], ([char, cb]: any, env) => {
    global.lexicalEnv.setFrom(char, async (locals: any) => {
      const proc = await evaluate(cb, env);
      if (isProc(proc)) {
        mkNativeProc(proc.env, 'read', ['read'], ([locals]: any) => locals.parse());
        mkNativeProc(proc.env, 'advance', ['advance'], ([locals]: any) => locals.advance());
        mkNativeProc(proc.env, 'current', ['current'], ([locals]: any) => locals.current());
        mkNativeProc(proc.env, 'isEOF', ['isEOF'], ([locals]: any) => locals.isEOF());
        mkNativeProc(proc.env, 'isSpace', ['isSpace'], ([locals]: any) => locals.isSpace());
        mkNativeProc(proc.env, 'isNewLine', ['isNewLine'], ([locals]: any) => locals.isNewLine());
        return await evaluate([proc, locals, toString(char)], env);
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

  mkNativeProc(env, 'macroexpand', ['expr'], async ([args]: any) => await expand(args, true, env));

  const importableNameIndex = Util.searchIdx('stdlib', 'tests', 'samples')

  const parseLoadSymbol = (sym: symbol, ext = '.scm') => {
    const repr = sym.description
    assert(repr, 'A symbol has no name..')
    assert(
      repr.includes('/') &&
      repr.split('/')[0] in importableNameIndex,

      `Must import from a known namespace (eg: ${Object.keys(importableNameIndex).slice(0, 2).join(', ')}, etc.)`
    )
    return repr + ext
  }

  mkNativeProc(env, 'load', ['file'], async ([file]: any) => {
    if (Util.isSym(file)) {
      return await loadFile(parseLoadSymbol(file), global)
    }
    return await loadFile(file, global)
  });

  mkNativeProc(env, 'reload', ['file'], async ([file]: any) => {
    if (Util.isSym(file)) {
      return await loadFile(parseLoadSymbol(file), global, true)
    }
    return await loadFile(file, global, true)
  });

  // SRFI 6: Basic String Ports
  mkNativeProc(env, 'open-input-string', ['string'], ([string]: any) => {
    assert(typeof string === 'string')
    return InPort.fromString(string)
  });
  /// END SRFI 6

  // - 6.3.4 Characters
  mkNativeProc(env, 'char?', ['obj'], ([obj]: any) => {
    return Util.toL(Util.isChar(obj))
  });

  // 6.3.5 Strings
  mkNativeProc(env, 'string?', ['n'], ([n]: any) => Util.toL(Util.isString(n)));
  mkNativeProc(env, 'string', ['args'], ([args]: any) => args.join(''));
  mkNativeProc(env, 'make-string', ['k', 'char'], ([k, char = ' ']: any) => {
    assert(Util.isNum(k), 'make-string [arg(1)] expects a number')
    assert(Util.isChar(char), 'make-string [arg(2)] expects a char')
    return char.displayText.repeat(k)
  });
  mkNativeProc(env, 'string-length', ['n'], ([n]: any) => {
    Util.assert(Util.isString(n))
    return n.length
  });
  mkNativeProc(env, 'string-ref', ['string', 'k'], ([string, k]: any) => {
    Util.assert(Util.isString(string) && string.length >= k)
    return string[k]
  });
  mkNativeProc(env, 'string-set!', ['string', 'k', 'char'], ([string, k, char]: any) => {
    assert(Util.isString(string) && string.length > k)
    assert(Util.isChar(char))
    // string[k] = char.displayText
    return UNDEF
  });
  mkNativeProc(env, 'string->number', ['n', 'radix?'], ([n, radix]: any) => {
    Util.assert(Util.isString(n), `"string->number" procedure takes a 'string' as an argument`);
    return parseInt(n, radix ?? 10);
  });
  mkNativeProc(env, 'string->symbol', ['n'], ([n]: any) => {
    Util.assert(Util.isString(n), `"string->symbol" procedure takes a 'string' as an argument`);
    return Sym(n)
  });
  mkNativeProc(env, 'string->input-port', ['string'], ([string]: any) => {
    assert(typeof string === 'string')
    return InPort.fromString(string)
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

  // - 6.3.6 Vectors
  mkNativeProc(env, 'vector?', ['obj'], ([obj]: any) => {
    return Util.toL(Util.isVec(obj))
  });
  mkNativeProc(env, 'make-vector', ['k', 'fill?'], ([k, fill]: any) => {
    assert(fill === undefined, 'make-vector fill option not implemented')
    return new Vector(k)
  });
  mkNativeProc(env, 'vector', ['args'], ([args]: any) => {
    return new Vector(args)
  });
  mkNativeProc(env, 'vector->length', ['vec'], ([vec]: any) => {
    assert(Util.isVec(vec), `vector-length expected a Vector. Got: ${typeof vec}`)
    return (<Vector>vec).data.length
  });
  mkNativeProc(env, 'vector->ref', ['vec', 'k'], ([vec, k]: any) => {
    assert(Util.isVec(vec), `vector-ref [arg(1)] expected a Vector. Got: ${typeof vec}`)
    assert(Util.isNumber(k), `vector-ref [arg(2)] expected a Number. Got: ${typeof vec}`)
    return (<Vector>vec).data[k]
  });
  mkNativeProc(env, 'vector->set!', ['vec', 'k', 'obj'], ([vec, k, obj]: any) => {
    assert(Util.isVec(vec), `vector-ref [arg(1)] expected a Vector. Got: ${typeof vec}`)
    assert(Util.isNumber(k), `vector-ref [arg(2)] expected a Number. Got: ${typeof vec}`)
    assert(obj, `vector-ref [arg(3)] is undefined`)
    return (<Vector>vec).data[k] = obj
  });
  mkNativeProc(env, 'vector->list', ['vec'], ([vec]: any) => {
    assert(Util.isVec(vec), `vector-list expected a Vector. Got: ${typeof vec}`)
    return (<Vector>vec).data
  });
  mkNativeProc(env, 'list->vector', ['list'], ([list]: any) => {
    assert(Util.isList(list), `list->vector expected a list. Got: ${typeof list}`)
    return new Vector(list)
  });
  mkNativeProc(env, 'vector->fill!', ['vec', 'fill'], ([vec, fill]: any) => {
    assert(Util.isVec(vec), `vector-list expected a Vector. Got: ${typeof vec}`)
    assert(fill, `vector-list [arg(2)] expected an argument`)
    for (let i = 0; i < vec.data.length; i++) {
      vec.data[i] = fill
    }
  });

  // - 6.4 Control Features
  mkNativeProc(env, 'procedure?', ['obj'], ([obj]: any) => {
    return Util.toL(isProc(obj) || isNativeProc(obj))
  });

  // 6.6 Input and output

  // - 6.6.1 Ports
  mkNativeProc(env, 'call-with-input-file', ['string', 'proc'], ([string, proc]: any) => {
    throw new NotImplementedError('call-with-input-file')
  });

  mkNativeProc(env, 'input-port?', ['obj'], ([obj]: any) => Util.toL(isInputPort(obj)));
  mkNativeProc(env, 'output-port?', ['obj'], ([obj]: any) => Util.toL(isOutputPort(obj)));

  mkNativeProc(env, 'current-input-port', [], () => currentInputPort(global));
  mkNativeProc(env, 'current-output-port', [], () => currentOutputPort(global));

  mkNativeProc(env, 'set-current-input-port!', ['port'], ([port]: any) => env.set('*current-input-port*', port));
  mkNativeProc(env, 'set-current-output-port!', ['port'], ([port]: any) => env.set('*current-output-port*', port));

  mkNativeProc(env, 'open-input-file', ['filename'], ([filename]: any) => {
    assert(typeof filename === 'string')
    return InPort.fromFile(filename)
  });

  mkNativeProc(env, 'open-output-file', ['filename'], ([filename]: any) => {
    assert(typeof filename === 'string')
    return OutPort.fromFile(filename)
  });

  // Sockets
  mkNativeProc(env, 'socket-client->input-port', ['address'], ([address]: any) => {
    assert(typeof address === 'string')
    return InPort.fromSocketClient(address)
  });
  mkNativeProc(env, 'socket-client->output-port', ['address'], ([address]: any) => {
    assert(typeof address === 'string')
    return OutPort.fromSocketClient(address)
  });
  mkNativeProc(env, 'socket-server->input-port', ['address'], ([port]: any) => {
    assert(typeof port === 'string' || typeof port === 'number')
    return InPort.fromSocketServer(port)
  });
  mkNativeProc(env, 'socket-server->output-port', ['address'], ([port]: any) => {
    assert(typeof port === 'string' || typeof port === 'number')
    return OutPort.fromSocketServer(port)
  });

  // - 6.6.2 Input
  mkNativeProc(env, 'read', ['port'], async ([port]: any) => {
    const p: InPort = port ?? currentInputPort(global)
    // console.log('reading port', p.name)
    const data = await read(p, readerEnv);
    // console.log('reading port (data):', data)
    return data
  });

  mkNativeProc(env, 'read-char', ['port'], async ([port]: any) => {
    const p: InPort = port ?? currentInputPort(global)
    return await p.readChar()
  });

  mkNativeProc(env, 'peek-char', ['port'], async ([port]: any) => {
    const p: InPort = port ?? currentInputPort(global)
    return await p.peekChar()
  });

  mkNativeProc(env, 'eof-object?', ['obj'], ([obj]: any) => Util.toL(isEofString(obj)));

  mkNativeProc(env, 'char-ready?', ['port'], ([port]: any) => {
    const p: InPort = port ?? currentInputPort(global)
    return Util.toL(isEofString(p) || p.charReady())
  });

  // - 6.6.3 Output
  mkNativeProc(env, 'putchar', ['char', 'port?'], ([obj, port]: any) => {
    const p: OutPort = port ?? currentOutputPort(global)
    p.write(obj)
    return
  });
  mkNativeProc(env, 'write', ['obj', 'port?'], ([obj, port]: any) => {
    const p: OutPort = port ?? currentOutputPort(global)
    p.write(obj)
    return
  });
  mkNativeProc(env, 'display', ['obj', 'port?'], ([obj, port]: any) => {
    const p: OutPort = port ?? currentOutputPort(global)
    p.write(obj)
    return
  });
  mkNativeProc(env, 'newline', ['port?'], ([port]: any) => {
    const p: OutPort = port ?? currentOutputPort(global)
    p.write('\n')
    return
  });
  mkNativeProc(env, 'write-char', ['char', 'port?'], ([char, port]: any) => {
    assert(Util.isChar(char), `not a character: ${char}`)
    const p: OutPort = port ?? currentOutputPort(global)
    p.write(char.displayText)
    return
  });

  mkNativeProc(env, 'putchar2', ['char1', 'char2', 'port?'], ([obj1, obj2, port]: any) => {
    const p: OutPort = port ?? currentOutputPort(global)
    p.write(obj1)
    p.write(obj2)
    return
  });

  mkNativeProc(env, 'resume-from-error', [], () => {
    throw new Resume()
  })

  mkNativeProc(env, 'exit-repl', [], () => {
    if (Util.isT(global.env.get('*in-repl-mode*')))
      global.env.set('*in-repl-mode*', Util.toL(false))
    else
      throw new Error('not in repl mode');
  })

  mkNativeProc(env, 'set-current-repl-prompt!', ['prompt'], ([prompt]: any) => {
    global.env.set('*current-repl-prompt*', prompt)
  })

  mkNativeProc(env, 'get-current-repl-prompt', [], () => {
    return global.env.get('*current-repl-prompt*')
  })

  mkNativeProc(env, 'revert-to-default-repl-prompt!', [], () => {
    global.env.set('*current-repl-prompt*', global.env.get('*default-repl-prompt*'))
  })

  // (define p (socket-server->input-port 9002))
  // (set-current-input-port! p)
  // (set-current-repl-prompt! "")
  // (set-current-input-port! *default-input-port*)
  // (set-current-repl-prompt! "> ")

  // (define p (socket-server->input-port 9002)) (set-current-input-port! p)

  mkNativeProc(env, 'repl', [], async () => {
    global.env.set('*in-repl-mode*', Util.toL(true))
    let lastOutput: any, lastInput: any;
    while (Util.isT(global.env.get('*in-repl-mode*'))) {
      const p = currentInputPort(global)
      const o = currentOutputPort(global)
      try {
        o.write(global.env.get('*current-repl-prompt*'))
        lastInput = await read(p, global.readerEnv);
        lastInput = await expand(lastInput, true, global.lexicalEnv)
        lastOutput = await evaluate(lastInput, global.env)
        const r = toStringSafe(lastOutput);
        if (r !== undefined) o.write(r)
        o.write('\n')
      } catch (err) {
        if (err instanceof Resume) {
          global.env.set('*in-repl-mode*', Util.toL(false))
          throw err
        }
        if (err instanceof Error) {
          // if (err instanceof MissingParenthesisError) {
          //   multiline = true
          //   continue
          // }
          if (err instanceof UndefinedVariableError) {
            if (err.message.endsWith('#<eof-object>')) {
              continue
            }
          }
          o.write(err.message + '\n')
        }
        console.log('error: ' + err)
      }
    }
  });
}
