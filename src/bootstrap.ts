
// const isAtom = (expr: LispExpr): expr is LispAtom => typeof expr === 'string'
// const is = (e: LispExpr): boolean => e === TRUE

// const TRUE:  LispAtom = '#t'
// const NIL:   LispAtom = '#nil';
// const EMPTY: ConsCell = { car: NIL, cdr: NIL };

// const isEmpty = (x: LispExpr): boolean => JSON.stringify(x) === JSON.stringify(EMPTY)

// // primitives
// const quote = (expr: LispExpr): LispExpr => expr
// const atom = (expr: LispExpr): LispExpr => isAtom(expr) ? TRUE : EMPTY
// const eq = (x: LispExpr, y: LispExpr): LispExpr => {
//   const equalAtoms = isAtom(x) && isAtom(y) && x === y;
//   const bothEmpty = isEmpty(x) && isEmpty(y);
//   return (equalAtoms || bothEmpty) ? TRUE : EMPTY
// }
// const car = (expr: LispExpr): LispExpr => !isAtom(expr) ? expr.car : EMPTY
// const cdr = (expr: LispExpr): LispExpr => !isAtom(expr) ? expr.cdr : EMPTY
// const cons = (car: LispExpr, cdr: LispExpr): ConsCell => ({ car, cdr })
// // const cond = (...exprs: [p: LispExpr, e: LispExpr][]): LispExpr => {
// //   for (const [p, e] of exprs) {
// //     if (eq(p, TRUE)) return e
// //   }
// //   return NIL
// // }

// // functions
// const cadr   = (expr: LispExpr): LispExpr => car(cdr(expr))
// const cdar   = (expr: LispExpr): LispExpr => cdr(car(expr))
// const caar   = (expr: LispExpr): LispExpr => car(car(expr))
// const cadar  = (expr: LispExpr): LispExpr => car(cdr(car(expr)))
// const caddr  = (expr: LispExpr): LispExpr => car(cdr(cdr(expr)))
// const caddar = (expr: LispExpr): LispExpr => car(cdr(cdr(car(expr))))
// const list = (...exprs: LispExpr[]): ConsCell => {
//   // exprs
//   return exprs.reduceRight((acc: any, expr) => cons(expr, acc), EMPTY) as ConsCell
// }
// const nil = (x: LispExpr): LispExpr => {
//   // x
//   return eq(x, EMPTY)
// }
// const and = (x: LispExpr, y: LispExpr): LispExpr => {
//   if (is(x) && is(y)) return TRUE
//   return EMPTY
//   // cond(
//   //   [x, cond([y, TRUE], [TRUE, EMPTY])],
//   //   [TRUE, EMPTY],
//   // )
// }
// const not = (x: LispExpr): LispExpr => {
//   if (is(x)) return EMPTY
//   return TRUE
//   // cond([x, EMPTY], [TRUE, TRUE])
// }
// const append = (x: LispExpr, y: ConsCell): ConsCell => {
//   if (is(nil(x))) {
//     return y
//   }
//   return cons(car(x), append(cdr(x), y))
//   // cond(
//   //   [nil(x), y],
//   //   [TRUE, cons(car(x), append(cdr(x), y))],
//   // )
// }
// const pair = (x: LispExpr, y: LispExpr): ConsCell => {
//   if (is(nil(x)) && is(nil(y)))
//     return EMPTY

//   if (is(not(atom(x))) && is(not(atom(y)))) {
//     x
//     y
//     return cons(list(car(x), car(y)),
//                 pair(cdr(x), cdr(y)))
//   }

//   console.log(is(not(atom(x))))
//   console.log(is(not(atom(y))))

//   x
//   y
//   throw new Error('Dunno how I got here!')
//   // return <ConsCell>cond(
//   //   [and(nil(x), nil(y)), EMPTY],
//   //   [and(not(atom(x)), not(atom(y))),
//   //     cons(list(car(x), car(y)),
//   //         pair(cdr(x), cdr(y)))],
//   // )
// }
// const assoc = (x: LispExpr, y: LispExpr): LispExpr => {
//   if (is(nil(cdr(y)))) return y
//   if (is(eq(caar(y), x))) cadar(y)
//   return assoc(x, cdr(y))
//   // return cond([eq(caar(y), x), cadar(y)],
//   //             [TRUE, assoc(x, cdr(y))])
// }

// // lib
// const read = (expr: string): LispExpr => {
//   let cursor = 0, end = expr.length - 1

//   const advance = () => expr[cursor++]
//   const current = () => expr[cursor]
//   const isOpenS = () => expr[cursor] === '('
//   const isCloseS = () => expr[cursor] === ')'
//   const isSpace = () => expr[cursor] === ' '
//   const isAlpha = (c: string) => (((c >= 'a') && (c <= 'z')) || ((c >= 'A') && (c <= 'Z')))
//   const isDigit = (c: string) => ((c >= '0') && (c <= '9'))
//   const isAlnum = (c: string) => isAlpha(c) || isDigit(c)
//   const atEnd = () => cursor > end

//   function parseAtom(): LispExpr {
//     let res: LispExpr[] = []
//     while (isAlnum(current())) {
//       res.push(advance())
//     }
//     return res.join('')
//   }

//   function parseLambda(): LispExpr {
//     let atom = parseAtom()
//     if (atom === 'defun') {
//       const name = parse();
//       name
//       const fn = cons(cons('lambda', cons(parse(), EMPTY)), EMPTY);
//       fn
//       const lambda = cons('label', cons(name, fn))
//       lambda
//       return lambda
//       // return cons('label', cons(name, cons('lambda', cons(parse(), cons(parse(), EMPTY)))))
//     }
//     return atom
//   }

//   function parseQuote(): LispExpr {
//     if (current() === "'") {
//       advance()
//       return list('quote', parse())
//     }

//     return parseLambda()
//   }

//   function parseList(): LispExpr {
//     if (isOpenS()) {
//       advance()
//       let exprs: LispExpr[] = [], idx = 0

//       while (!isCloseS() && !atEnd()) {
//         exprs[idx++] = parse()
//       }

//       if (!atEnd()) advance()
//       else throw new Error("Missing ')'")

//       return exprs.reduceRight((cdr, car) => {
//         return cons(car, cdr as ConsCell)
//       }, EMPTY)
//     }

//     return parseQuote()
//   }

//   function parse(): LispExpr {
//     while (isSpace()) advance()
//     return parseList()
//   }

//   return parse()
// }

// const _eval = (e: LispExpr, a: ConsCell): LispExpr => {
//   if (is(atom(e))) return assoc(e, a)
//   if (is(atom(car(e)))) switch (car(e)) {
//     case 'quote': return cadr(e)
//     case 'atom':  return atom(_eval(cadr(e), a))
//     case 'eq':    return eq(_eval(cadr(e), a),
//                             _eval(caddr(e), a))
//     case 'car':   return car(_eval(cadr(e), a))
//     case 'cdr':   return cdr(_eval(cadr(e), a))
//     case 'cons':  return cons(_eval(cadr(e), a),
//                               _eval(caddr(e), a))
//     case 'cond':  return eval_cond(cdr(e), a)
//     default: {
//       return _eval(cons(assoc(car(e), a),
//                                cdr(e)), a)
//     }
//   }
//   if (is(eq(caar(e), 'label'))) {
//     // e
//     // console.log(car(e))
//     // console.log(cdr(e))
//     // console.log(caddar(e))
//     const lambda = cons(caddar(e), cdr(e));
//     console.log(lambda)
//     const env = cons(list(cadar(e), car(e)), a);
//     console.log(env)
//     return _eval(lambda, env)
//   }
//   if (is(eq(caar(e), 'lambda'))) {
//     e
//     const paramMaybe = caddar(e);
//     paramMaybe
//     const cdre = cdr(e);
//     cdre
//     const elr = eval_list(cdre, a);
//     elr
//     const erg = cadar(e);
//     erg
//     const entry = pair(erg, elr);
//     entry
//     const env = append(entry, a);
//     env
//     return _eval(paramMaybe, env)
//   }
//   e
//   throw new Error('Fallthrough')
// }
// const eval_cond = (c: LispExpr, a: ConsCell): LispExpr => {
//   if (_eval(caar(c), a))
//     return _eval(cadar(c), a)
//   return eval_cond(cdr(c), a)
// }
// const eval_list = (m: LispExpr, a: ConsCell): ConsCell => {
//   if (nil(m)) return EMPTY
//   return cons(_eval(car(m), a), eval_list(cdr(m), a))
// }

// const toString = (e: LispExpr): string => {
//   if (e === null) {
//     throw new Error('NULL string')
//   }
//   if (is(atom(e))) return String(e)
//   if (is(nil(e)))  return '()'
//   if (is(atom(car(e)))) {
//     switch (car(e)) {
//       case 'quote':  return `'${toString(cadr(e))}`
//     }
//   }
//   return `(${toString(car(e))} ${toString(cdr(e))})`
// }

// const print = (e: LispExpr): void => console.log(toString(e))

// // testing

// // console.log(toString(
// //   read("((a b) (c d) e)")
// // ))

// // console.log(toString(
// //   read("(quote (a b c d e))")
// // ))

// // console.log(toString(
// //   read("'(a b c d e)")
// // ))

// // console.log(toString(
// //   list(list('a', 'b'), list('c', 'd'), 'e')
// // ))

// // console.log(
// //   read("(eq '() '())")
// // )
// // console.log(toString(
// //   read("(eq '() '())")
// // ))

// // console.log(toString(
// //   _eval(read("(eq '() '())"), EMPTY)
// // ))

// // console.log(toString(
// //   read("(lambda (x y) eq(x y))")
// // ))

// console.log(_eval(
//   read("(defun subst (x y) (eq(x y)))"), EMPTY
// ))

// // console.log(toString(
// //   _eval(read("((lambda (x y) eq(x y)) ('a 'a))"), EMPTY)
// // ))

// function testRead() {
//   // var rs = parse('1');
//   // var rs = parse('(1)');
//   // var rs = parse('(1 2 3 4)');
//   // var rs = parse('1 2');
//   // var rs = parse('(1 (2 3 4 5))');
//   var rs = read('(1 2 3 4)');
//   // var rs = parse('(1 (2 (3 4)))');
//   // var rs = parse('(1 (2 (3 nil)))');
//   // var rs = parse('(nil)');
//   console.log(toString(rs))
// }

// // testRead()
