import { Term } from "./terms";
import * as Utils from "../utils";
import { Position } from "../types";
import { SyntaxRulesDef } from "./syntax";

export class InvalidEnvArgumentsError extends Error {
  constructor(
    public params: Term,
    public args: Term
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
  constructor(expr: Term) {
    super(`Error: expression is not callable: ${Utils.toString(expr, true)}`);
  }
}
export class UnexpectedParenthesisError extends Error {
  constructor(source: string, options: FormatErrorOptions) {
    super(`Error: Unexpected ")" @ Ln ${options.end.line}, Col ${options.end.col})`);
    this.formattedError = formatError(source, { ...options, message: this.message });
  }
  public formattedError: string;
}
export class MissingParenthesisError extends Error {
  constructor(source: string, options: FormatErrorOptions) {
    super(`Error: Missing ')' @ Ln ${options.end.line}, Col ${options.end.col})`);
    this.formattedError = formatError(source, { ...options, message: this.message });
  }
  public formattedError: string;
}
export class MalformedStringError extends Error {
  constructor(source: string, options: FormatErrorOptions) {
    super(`Error: Missing '"' @ Ln ${options.end.line}, Col ${options.end.col})`);
    this.formattedError = formatError(source, { ...options, message: this.message });
  }
  public formattedError: string;
}
export class MatchError extends Error {
  constructor(
    public def: SyntaxRulesDef,
    public form: Term,
  ) {
    super(`no matches found for pattern: ${Utils.toStringSafe(form)}`)
  }
}

export class InputError extends Error {
  constructor(
    public def: SyntaxRulesDef,
    public form: Term,
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
