import assert from "assert";

const Tag = {
  REG: 'regular-id',
  PAT: 'pattern-id',
  LIT: 'literal-id',
  DAT: 'datum',
  DOTS: 'ellipsis-template',
  LIST: 'tlist',
}

export class Env {
  constructor(params: any = [], args: any = [], public outer?: Env) {
    if (Array.isArray(params) && Array.isArray(args)) {
      const getParams = (params: any[], args: any[]): [string, any][] => {
        if (params.length === 0) return []
        if (params[0] === '...') {
          if (params.slice(1).length !== 0) {
            debugger
          }
          return [[params[0], args]]
        }
        if (params[0] === '.') {
          return [[params[1], args]]
        }
        const [x0, ...xs0] = params
        const [x1, ...xs1] = args
        return [[x0, x1], ...getParams(xs0, xs1)]
      }
      const formals = getParams(params, args);
      this.inner = Object.fromEntries(formals);
    } else if (typeof params === 'string') {
      this.inner = { [params]: args };
    }
    else {
      throw new Error(`${{params, args}}`);
    }
  }
  getFrom<T extends any>(expr: any): T {
    return this.inner[expr]
  }
  private inner: any = {}
}

class Test {
  genOutput(template: any, env: Env): any {
    assert(Array.isArray(template), '`genOutput` expected an arrray')
    switch (template[0]) {
      case Tag.LIST: {
        const [_def, patterns] = template
        let values: any[] = []

        for (const ptn of <any[]>patterns) {
          const output = this.genOutput(ptn, env);

          switch (ptn[0]) {
            case Tag.DOTS:
              if ((output).length > 0)
                values.push(...output)
              break
            // regular-id
            case Tag.REG:
            // pattern-id
            case Tag.PAT:
              values.push(output)
              break
            // literal-id
            case Tag.LIT:
            // datum
            case Tag.DAT:
            case Tag.LIST:
            default: {
              values.push(output)
              break
            }
          }
        }

        return values
      }
      case Tag.DOTS: {
        const [___, pat] = <any>template
        let items = this.genOutput(pat, env);
        if (pat[0] === Tag.LIST) {
          return (<any[]>items).map(item => item[0])
        }

        return items
      }

      // literal-id
      case Tag.LIT:
      // datum
      case Tag.DAT:
        return template[1]
      // regular-id
      case Tag.REG:
      // pattern-id
      case Tag.PAT: {
        const id = template[1]
        return env.getFrom(id);
      }

      default: {
        throw new Error();
      }
    }
  }
}


export const zip = (...rows: any[][]) => (rows.length === 0) ? [[], []] : rows[0].map((_, c) => rows.map(row => row[c]));

const r = zip(...Object.entries({
  name1: ['a', 'b'],
  name2: ['c', 'd'],
  body1: [['*', 'z', 'z']],
  body2: [[]],
  "print.1": 'print',
}));

const e = new Env(...r)
const t = new Test()

console.log(zip(['abc']));
console.log(zip(['abc'], [1]));
console.log(zip(['a', 'b'], [1, 2]));

console.log(t.genOutput([Tag.LIST, []], e));

console.log(t.genOutput(
  [Tag.LIST,
    [[Tag.REG, "print.1"],
     [Tag.LIST, [[Tag.DOTS, [Tag.LIST, [[Tag.PAT, 'name1'], [Tag.PAT, 'name2']]], 1]]],
     [Tag.PAT, 'body1'],
     [Tag.DOTS, [Tag.PAT, 'body2'], 1],
  ]], e));


console.log(
  t.genOutput([Tag.LIST, [[Tag.DOTS, [Tag.PAT, 'name1'], [Tag.DOTS, [Tag.PAT, 'name1']]]]], e));
