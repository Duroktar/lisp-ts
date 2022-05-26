export type token =
  | identifier | bool | character | number | string
  | '(' | ')' | '#(' | "'" | '`' | ',' | ',@' | '.'

export type delimiter = whitespace | '(' | ')' | '"' | ';'
export type whitespace = ' ' | '\t' | '\n'

export type identifier = `${initial}${subsequent|""}` | peculiarIdentifier
export type initial = letter | specialInitial
export type letter = 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h'
  | 'i' | 'j' | 'k' | 'l' | 'm' | 'n' | 'o' | 'p' | 'q'
  | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z'
export type specialInitial = '!' | '$' | '%' | '&' | '*' | '/'
  | ':' | '<' | '=' | '>' | '?' | '~' | '_' | '^'
export type subsequent = initial | digit | specialSubsequent
export type digit = '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
export type specialSubsequent = '+' | '-' | '.' | '@'
export type peculiarIdentifier = '+' | '-' | '...'
export type syntacticKeyword = expressionKeyword
  | 'else' | '=>' | 'define'
  | 'unquote' | 'unquote-splicing'
export type expressionKeyword = 'quote' | 'lambda' | 'if'
  | 'set!' | 'begin' | 'cond' | 'and' | 'or' | 'case'
  | 'let' | 'let*' | 'letrec' | 'do' | 'delay'
  | 'quasiquote'

export type variable = Exclude<identifier, syntacticKeyword>

export type bool = '#t' | '#f'
export type character = `#\\${letter | characterName}`
export type characterName = 'space' | 'newline'
