import * as Utils from "../utils";
import { Env } from "./env";
import { evaluate } from "./eval";
import { expand } from "./expand";
import { read } from "./read";
import { Expr, List } from "./terms";

// primitives (7)
export const quote = (expr: Expr): Expr => cadr(expr);
export const atom = (expr: Expr): Expr => Utils.toL(Utils.isAtom(expr));
export const eq = (x: Expr, y: Expr): Expr => Utils.toL(Utils.isSym(x) && Utils.isSym(y) && x === y || Utils.isEmpty(x) && Utils.isEmpty(y));
export const cons = (car: Expr, cdr: Expr): Expr => [car, ...<any>Utils.expect(cdr, Utils.isList, '2nd Argument to cons must be an array...')];
export const car = (expr: Expr): Expr => Utils.expect(<any>expr, Utils.isList, 'Argument to car must be an array..')[0];
export const cdr = (expr: Expr): Expr => Utils.expect(<any>expr, Utils.isList, 'Argument to cdr must be an array..').slice(1);
// END primitives

// export const compose = (...fns: Function[]) => (arg: any) => fns.reduceRight((acc, fn) => fn(acc), arg)
// export const cadr    = compose(car, cdr)

// functions
export const cadr    = (expr: Expr): Expr => car(cdr(expr));
export const cdar    = (expr: Expr): Expr => cdr(car(expr));
export const caar    = (expr: Expr): Expr => car(car(expr));
export const cadar   = (expr: Expr): Expr => car(cdr(car(expr)));
export const caddr   = (expr: Expr): Expr => car(cdr(cdr(expr)));
export const cdadr   = (expr: Expr): Expr => cdr(car(cdr(expr)));
export const caddar  = (expr: Expr): Expr => car(cdr(cdr(car(expr))));
export const cadddr  = (expr: Expr): Expr => car(cdr(cdr(cdr(expr))));
export const cadadr  = (expr: Expr): Expr => car(cdr(car(cdr(expr))));
export const cadddar = (expr: Expr): Expr => car(cdr(cdr(cdr(car(expr)))));

export const _do = (args: Expr[], env: Env) => {
  /*
  (do ((<variable1> <init1> <step1>) ...)
        (<test> <expression> ...)
      <command> ...)
  */
  const [[...preludes], [test, ...expressions], ...commands] = <any>args;

  // Syntax: The <init>s, <step>s, <test>s, and <command>s must be expressions.
  //         The <variable>s must be pairwise distinct variables.
  preludes.forEach(([var1, init1, step1]: any) => {
    Utils.expect(var1, Utils.isAtom(var1));
    Utils.expect(var1, Utils.isExpr(init1));
    Utils.expect(var1, Utils.isExpr(step1));
  });
  Utils.expect(test, Utils.isExpr(test), `Test must be an expression. Got: ${typeof test} .. (value: ${Utils.toString(test)})`);
  expressions.forEach((expression: any) => {
    Utils.expect(expression, Utils.isExpr(expression), `Not an expression. Got: ${typeof test} .. (value: ${Utils.toString(test)})`);
  });
  commands.forEach((command: any) => {
    Utils.expect(command, Utils.isExpr(command), `A Command must be an expression. Got: ${typeof command} .. (value: ${Utils.toString(command)})`);
  });

  const bindings: List = [];
  const steps: List = [];

  // The <init> expressions are evaluated (in some unspecified order),
  // the <variable>s are bound to fresh locations,
  // the results of the <init> expressions are stored in the bindings of the <variable>s,
  // and then the iteration phase begins.
  preludes.forEach(([var1, init1, step1]: any) => {
    bindings.push([Utils.toString(var1), evaluate(init1, env)]);
    steps.push([var1, step1]);
  });
  bindings.forEach(([varName, binding]: any) => {
    env.set(varName, binding);
  });

  // Each iteration begins by evaluating <test>;
  // If the result is #f, then the <command>s are evaluated in order for effect,
  // - the <step> expressions are evaluated in some unspecified order,
  // - the <variable>s are bound to fresh locations holding the results,
  // Then the next iteration begins.
  const iterate = (depth = 0): Expr => {
    const testResult = evaluate(test, env);
    if (Utils.isT(testResult) === false) {
      commands.forEach((command: any) => {
        evaluate(command, env);
      });
      steps.forEach((step: any) => {
        const [varName, result] = step;
        const res = evaluate(result, env);
        env.set(Utils.toString(varName), res);
      });
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
      return expressions.reduce((_: any, expression: any) => {
        return evaluate(expression, env);
      }, []);
    }
  };

  return iterate();
}

export const parse = (code: string, a: Env): Expr => {
  return expand(read(code), true, a);
};
export const execute = (code: string, a: Env): Expr => {
  return evaluate(parse(code, a), a);
};
