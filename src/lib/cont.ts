import { isCallable, mkNativeFunc } from "../utils";
import { Env } from "./env";
import { InvalidCallableExpression } from "./error";
import { Expr } from "./terms";

export const callWithCC = ([proc]: any, env: Env) => {
  class RuntimeWarning extends Error { public retval?: any; }
  let ball = new RuntimeWarning("Sorry, can't continue this continuation any longer.");
  const throw_ = mkNativeFunc(env, 'throw', ['retval'], ([retval]: any) => {
    ball.retval = retval; throw ball;
  });
  try {
    if (isCallable(proc)) {
      return proc.call([throw_ as Expr]);
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
