import type { Form } from "./forms";
import type { SyntaxRulesDef } from "./syntax";
import { toString, toStringSafe } from "./toString";

export type Position = {
  line: number;
  col: number;
  cursor: number;
};

export class InvalidEnvArgumentsError extends Error {
  constructor(
    public params: Form,
    public args: Form
  ) {
    super('Error: Invalid Env params');
  }
}
export class UndefinedVariableError extends Error {
  constructor(name: string) {
    super(`Error: undefined variable: ${name}`);
  }
}
export class InvalidCallableExpression extends Error {
  constructor(expr: Form) {
    super(`Error: expression is not callable: ${toString(expr, true)}`);
  }
}
export class UnexpectedParenthesisError extends Error {
  constructor() {
    super(`Error: Unexpected ")"`);
  }
}
export class MissingParenthesisError extends Error {
  constructor() {
    super(`Error: Missing ')'`);
  }
}
export class MalformedStringError extends Error {
  constructor() {
    super(`Error: Missing '"'`);
  }
}
export class MatchError extends Error {
  constructor(
    public def: SyntaxRulesDef,
    public form: Form,
  ) {
    super(`no matches found for pattern: ${toStringSafe(form)}`)
  }
}

export class NotImplementedError extends Error {}

export class InputError extends Error {
  constructor(
    public def: SyntaxRulesDef,
    public form: Form,
  ) {
    super()
  }
}

export type FormatErrorOptions = {
  message?: string;
  start: Position;
  end: Position;
};

export function formatError(source: string, options: FormatErrorOptions) {
  const lines = source.split('\n');
  const res: string[] = [];
  for (let lineNo = lines.length - 1; lineNo >= 0; lineNo--) {
    if (options.end.line === options.start.line) {
      const fst = '^'.padStart(options.start.col, ' ');
      const snd = '^'.padStart(options.end.col - (options.start.col + 1), ' ');
      if (lineNo === options.end.line - 1) {
        res.push(`${fst}${snd}`);
      }
    }
    else if (lineNo === options.start.line - 1) {
      res.push('^'.padStart(options.start.col, ' '));
    }
    else if (lineNo === options.end.line - 1) {
      res.push('^'.padStart(options.end.col, ' '));
    }
    res.push(lines[lineNo]);
  }
  return `${options.message}\n\n${res.reverse().join('\n')}`;
}
