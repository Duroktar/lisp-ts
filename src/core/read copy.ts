// import type { bool, character, delimiter, digit, identifier, initial, letter, subsequent, token, whitespace } from "../syntax";
// import { isString } from "../utils";
// import { delimiters, EOF, isPeculiarIdentifier, isSpecialInitial, isSpecialSubsequent } from "./const";
// import { quotes } from "./macro";
// import type { InPort } from "./port";
// import { Sym } from "./sym";
// import type { Term } from "./terms";

// export const read = (port: InPort): Term => {
//   function readAhead(token: token): any {
//     if (token === '(') {
//       const L: any[] = []
//       while (true) {
//         token = port.readToken()
//         if (token === ')') return L
//         else L.push(readAhead(token))
//       }
//     }
//     else if (token === ')')
//       throw new Error('unexpected ")"')
//     else if (quotes[token] !== undefined)
//       return [quotes[token], read(port)]
//     else if (token === EOF)
//       throw new Error('unexpected "EOF"')
//     else
//       return atom(token)
//   }

//   const nextChar = port.readToken()
//   return isEofObject(nextChar) ? EOF : readAhead(nextChar)
// };

// const atom = (token: any) => {
//   if (isBoolean(token))
//     return Sym(token)
//   else if (isNumeric(token))
//     return Number(token)
//   else if (token[0] === '"')
//     return token.slice(1, -1)
//   else
//     return Sym(token)
// }

// export const isNumeric = (num: any) => (typeof(num) === 'number' || typeof(num) === "string" && num.trim() !== '') && !isNaN(num as number);
// export const isEofObject = (obj: any) => obj === EOF
// export const isBoolean = (c: any): c is bool => !! (isString(c) && c.match(/^#[t|f]$/));
