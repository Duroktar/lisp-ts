
import * as Utils from "../utils";
import * as toString from "./toString";
import type { Env } from "./env";
import { evaluate } from "./eval";
import { expand } from "./expand";
import { InPort } from "./port";
import { read } from "./read";
import type { Term, List } from "./terms";
import { Environment } from "../env";
import { Resume } from "./cont";

// primitives (7)
export const quote = (expr: Term): Term => (<List>expr)[1];
export const atom = (expr: Term): Term => Utils.toL(Utils.isAtom(expr));
export const eq = (x: Term, y: Term): Term => Utils.toL(Utils.isSym(x) && Utils.isSym(y) && x === y || Utils.isEmpty(x) && Utils.isEmpty(y));
export const car = (expr: Term): Term => Utils.expect(<any>expr, Utils.isList, 'Argument to car must be an array..')[0];
export const cdr = (expr: Term): Term => Utils.expect(<any>expr, Utils.isList, 'Argument to cdr must be an array..').slice(1);
// END primitives

export const _do = async (args: Term[], env: Env) => {
  /*
  (do ((<variable1> <init1> <step1>) ...)
        (<test> <expression> ...)
      <command> ...)
  */
  const [[...preludes], [test, ...expressions], ...commands] = <any[][]>args;

  // Syntax: The <init>s, <step>s, <test>s, and <command>s must be expressions.
  //         The <variable>s must be pairwise distinct variables.
  preludes.forEach(([var1, init1, step1]: any) => {
    Utils.expect(var1, Utils.isAtom(var1));
    Utils.expect(var1, Utils.isExpr(init1));
    Utils.expect(var1, Utils.isExpr(step1));
  });
  Utils.expect(test, Utils.isExpr(test), `Test must be an expression. Got: ${typeof test} .. (value: ${toString.toString(test)})`);
  expressions.forEach((expression: any) => {
    Utils.expect(expression, Utils.isExpr(expression), `Not an expression. Got: ${typeof test} .. (value: ${toString.toString(test)})`);
  });
  commands.forEach((command: any) => {
    Utils.expect(command, Utils.isExpr(command), `A Command must be an expression. Got: ${typeof command} .. (value: ${toString.toString(command)})`);
  });

  let bindings: List = [];
  const steps: List = [];

  // The <init> expressions are evaluated (in some unspecified order),
  // the <variable>s are bound to fresh locations,
  // the results of the <init> expressions are stored in the bindings of the <variable>s,
  // and then the iteration phase begins.
  preludes.forEach(([var1, init1, step1]: any) => {
    bindings.push([var1, evaluate(init1, env)]);
    steps.push([var1, step1]);
  });

  processBindings();

  // Each iteration begins by evaluating <test>;
  // If the result is #f, then the <command>s are evaluated in order for effect,
  // - the <step> expressions are evaluated in some unspecified order,
  // - the <variable>s are bound to fresh locations holding the results,
  // Then the next iteration begins.
  const iterate = async (depth = 0): Promise<Term> => {
    const testResult = await evaluate(test, env);
    if (!Utils.isT(testResult)) {
      commands.forEach((command: any) => {
        evaluate(command, env);
      });
      steps.forEach((step: any) => {
        const [varName, result] = step;
        // const res = evaluate(result, env);
        // env.setFrom(varName, res);
        bindings.push([varName, evaluate(result, env)]);
      });
      processBindings()
      if (depth >= 1000) {
        throw new Error('max depth exceeded');
      }
      return iterate(depth + 1);
    } else {

      // If <test> evaluates to a true value;
      // - The <expression>s are evaluated from left to right,
      // and the values of the last <expression> are returned
      // - If no <expression>s are present,
      // then the do expression returns unspecified values
      const rv = expressions.map(expr => evaluate(expr, env));

      return rv.pop() ?? [];
    }
  };

  return await iterate();

  function processBindings() {
    while (bindings.length) {
      const [varName, binding] = bindings.shift() as any;
      env.setFrom(varName, binding);
    }
  }
}

export const tokenize = async (code: string, env: Environment): Promise<Term> => {
  return await read(InPort.fromString(code), env.readerEnv);
};

export const parse = async (code: string, {readerEnv}: Environment): Promise<Term> => {
  return await expand(await read(InPort.fromString(code), readerEnv), true, readerEnv);
};

export const execute = async (code: string, env: Environment): Promise<Term> => {
  const parsed = await parse(code, env);
  return await evaluate(parsed, env.env);
};

export const debugExecute = async (code: string, env: Environment): Promise<Term> => {
  try {
    // console.log('executing:', code)
    const result = await execute(code, env);
    // console.log('done executing:', result)
    return result;
  } catch (outerError) {
    if (outerError instanceof Error) {
      try {
        return await debugExecute(`
          (begin (
            (write ${JSON.stringify(outerError.message)})
            (newline)
            (write "Entering REPL...")
            (newline)
            (repl)))`, env)
      } catch (innerError) {
        if (innerError instanceof Resume) {
          return await debugExecute(code, env)
        }
        throw innerError
      }
    }
    throw outerError
  }
};
