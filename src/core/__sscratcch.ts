// import assert from "assert";

// type token =
//   | identifier | bool | character | number | string
//   | '(' | ')' | '#(' | "'" | '`' | ',' | ',@' | '.'

// type delimiter = whitespace | '(' | ')' | '"' | ';'
// type whitespace = ' ' | '\t' | '\n'

// type identifier = `${initial}${subsequent|""}` | peculiarIdentifier
// type initial = letter | specialInitial
// type letter = 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h'
//   | 'i' | 'j' | 'k' | 'l' | 'm' | 'n' | 'o' | 'p' | 'q'
//   | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z'
// type specialInitial = '!' | '$' | '%' | '&' | '*' | '/'
//   | ':' | '<' | '=' | '>' | '?' | '~' | '_' | '^'
// type subsequent = initial | digit | specialSubsequent
// type digit = '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
// type specialSubsequent = '+' | '-' | '.' | '@'
// type peculiarIdentifier = '+' | '-' | '...'
// type syntacticKeyword = expressionKeyword
//   | 'else' | '=>' | 'define'
//   | 'unquote' | 'unquote-splicing'
// type expressionKeyword = 'quote' | 'lambda' | 'if'
//   | 'set!' | 'begin' | 'cond' | 'and' | 'or' | 'case'
//   | 'let' | 'let*' | 'letrec' | 'do' | 'delay'
//   | 'quasiquote'

// type variable = Exclude<identifier, syntacticKeyword>

// type bool = '#t' | '#f'
// type character = `#\\${letter | characterName}`
// type characterName = 'space' | 'newline'

// ////////////////////////////////////////////////////////////////

// const specialInitials     = new Set('! $ % & * / : = < > ? ~ _ ^'.split(' '));
// const specialSubsequents  = new Set('+ - . @'.split(' '));
// const peculiarIdentifiers = new Set('+ - ...'.split(' '));
// const delimiters          = new Set('( ) " ;'.split(' '));

// const expressionKeywords  = new Set([
//   'quote', 'lambda', 'if', 'set!', 'begin',
//   'cond', 'and', 'or', 'case', 'let', 'let*',
//   'letrec', 'do', 'delay', 'quasiquote']);

// const syntacticKeywords = new Set([...expressionKeywords,
//   'else', '=>', 'define', 'unquote', 'unquote-splicing']);


// const isDelimiter = (c: any): c is delimiter => isWhiteSpace(c) || delimiters.has(c);
// const isWhiteSpace = (c: any): c is whitespace => c === ' ' || c === '\t' || c === '\n'
// const isIdentifier = (c: any): c is identifier => {
//   const [x, ...xs] = isString(c) ? c : []
//   return (isInitial(x) && xs.every(isSubsequent)) || isPeculiarIdentifier(c)
// };
// const isInitial = (c: any): c is initial => isLetter(c) || isSpecialInitial(c)
// const isLetter = (c: any): c is letter => !! (isString(c) && c.match(/^[A-z]*$/));
// const isSpecialInitial = (c: any): c is specialInitial => specialInitials.has(c);
// const isSubsequent = (c: any): c is subsequent => isInitial(c) || isDigit(c) || isSpecialSubsequent(c);
// const isDigit = (c: any): c is digit => !! (isString(c) && c.match(/^[0-9]*$/));
// const isSpecialSubsequent = (c: any): c is specialSubsequent => specialSubsequents.has(c);
// const isPeculiarIdentifier = (c: any): c is peculiarIdentifier => peculiarIdentifiers.has(c);
// const isSyntacticKeyword = (c: any): c is syntacticKeyword => syntacticKeywords.has(c);
// const isExpressionKeyword = (c: any): c is expressionKeyword => expressionKeywords.has(c);
// const isNumber = (c: any): c is number => typeof c === 'number';
// const isString = (c: any): c is string => typeof c === 'string';
// const isBoolean = (c: any): c is bool => !! (isString(c) && c.match(/^#[t|f]$/));
// const isCharacter = (c: any): c is character => !! (isString(c) && c.match(/^#\\([A-z]|(space|newline)){1}$/));

// namespace ScratchTests {
//   function tryProve<T>(fn: (t: T) => boolean, o: {proofs: T[], counters: any[]}): void {
//     o.proofs.forEach(p => assert(!!fn(p), `found proof that doesn't hold: ${p}`))
//     o.counters.forEach(c => assert(!fn(c), `found counter-proof that doesn't hold: ${c}`))
//   }

//   tryProve(isBoolean, {
//     proofs: ['#t', '#f'],
//     counters: ['#a', 'd34', '12kjh', '#t5', 234, undefined, null],
//   })

//   tryProve(isDigit, {
//     proofs: ['1', '3', '123', '6', ...'123456789'.split('')],
//     counters: ['a', 'd34', '12kjh', Infinity, 234, undefined, null],
//   })

//   tryProve(isLetter, {
//     proofs: ['asdf', 'f', 'HJKdkdfDFijf'],
//     counters: ['sdf4', '35df', '123', 234, undefined, null],
//   })

//   tryProve(isCharacter, {
//     proofs: ['#\\a', '#\\d', '#\\Z', '#\\space', '#\\newline'],
//     counters: ['#\\df3', '#\\4d', '35', 234, undefined, null],
//   })


//   tryProve(isIdentifier, {
//     proofs: [
//       'lambda'            , 'q',
//       'list->vector'      , 'soup',
//       '+'                 , 'V17a',
//       '<=?'               , 'a34kTMNs',
//       'the-word-recursion-has-many-meanings'
//     ],
//     counters: [undefined, null],
//   })
// }
