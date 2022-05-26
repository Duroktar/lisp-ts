
import * as Utils from "../utils";
import * as toString from "./toString";
import type { Env } from "./env";
import { evaluate } from "./eval";
import { expand } from "./expand";
import { InPort, RawText } from "./port";
import { read } from "./read";
import type { Term, List } from "./terms";

// primitives (7)
export const quote = (expr: Term): Term => (<List>expr)[1];
export const atom = (expr: Term): Term => Utils.toL(Utils.isAtom(expr));
export const eq = (x: Term, y: Term): Term => Utils.toL(Utils.isSym(x) && Utils.isSym(y) && x === y || Utils.isEmpty(x) && Utils.isEmpty(y));
export const car = (expr: Term): Term => Utils.expect(<any>expr, Utils.isList, 'Argument to car must be an array..')[0];
export const cdr = (expr: Term): Term => Utils.expect(<any>expr, Utils.isList, 'Argument to cdr must be an array..').slice(1);
// END primitives

export const _do = (args: Term[], env: Env) => {
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
  const iterate = (depth = 0): Term => {
    const testResult = evaluate(test, env);
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

  return iterate();

  function processBindings() {
    while (bindings.length) {
      const [varName, binding] = bindings.shift() as any;
      env.setFrom(varName, binding);
    }
  }
}

export const tokenize = (code: string, r: Env): Term => {
  return read(new InPort(new RawText(code)), r);
};

export const parse = (code: string, l: Env, r: Env): Term => {
  return expand(read(new InPort(new RawText(code)), r), true, l);
};

export const execute = (code: string, a: Env, l: Env, r: Env): Term => {
  const parsed = parse(code, l, r);
  // console.log('executing code', parsed);
  return evaluate(parsed, a);
};
