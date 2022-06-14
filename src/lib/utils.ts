import { isEmpty, isList } from "../guard";
import type { Form } from "../core/form";
import { list } from "../core/data/pair";
import { NativeProc } from "../core/data/proc";
import { Sym } from "../core/data/sym";
import type { iEnv } from "../interface/iEnv";

export function mkNativeProc(
  env: iEnv, name: string, params: string | string[],
  cb: (args: Form[] | Form, env: iEnv) => any, toArray = true): Form | NativeProc {

  const func = new class extends NativeProc {
    public name = name;
    public env = env;
    public params = Array.isArray(params) ? list(...params.map(Sym)) : Sym(params);
    public _call = (args: Form, env: iEnv) => cb(toArray ? (isList(args) ? (isEmpty(args) ? [] : args.toArray()) : args) : args, env);
  };

  env.set(name, func);
  return func;
}
