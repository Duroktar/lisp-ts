// import assert from "assert";

// function range(start: number, end: number) {
//   const r = []
//   for (let i = start; i < end; i++) {
//     r.push(i)
//   }
//   return r
// }
// function error(msg?: string): never { throw new Error(msg) }

// function evaluate(expression: any, scope: Scope): any {
//   throw new Error("Function not implemented.");
// }

// export const RESERVED = ['_', '...']

// type Scope = Map<string, Form>
// type Nil = symbol
// type List = Pair | Nil
// type Atom = string | number | symbol

// type Form = List | Atom | Binding | Callable | Expansion

// const nil = Symbol.for('()')
// export const ellipsis = Symbol.for('...')

// const Sym = Symbol.for
// const cons = (car: Form, cdr: Form = nil) => new Pair(car, cdr)
// const list = (...rv: Form[]) => rv.reduceRight<List>((acc, pair) => cons(pair, acc), nil);
// const car = (obj: any) => isPair(obj) ? obj.car : error('cdr')
// const cdr = (obj: any) => isPair(obj) ? obj.cdr : error('cdr')
// const cadr = (obj: any) => car(cdr(obj))

// export class Pair {
//   public parent!: any
//   constructor(
//     public car: Form,
//     public cdr: Form = nil,
//   ) {}
//   hosts(value: any) {
//     // if (isPair(value) && !isNil(value))
//     if (typeof value === 'object')
//       value.parent = this
//   }
//   *each(): any {
//     let pair: Form = this,
//         tail: List = nil;
//     while (isPair(pair)) {
//       yield(pair.car)
//       tail = pair
//       pair = pair.cdr
//     }
//     return tail
//   }
//   map(fn: (pair: any) => any) {
//     let pair: Form = this,
//         tail: List = nil,
//         rv: Form[] = [];

//     while (isPair(pair)) {
//       rv.push(fn(pair.car))
//       tail = pair
//       pair = pair.cdr
//     }
//     if (isPair(tail) && !isNil(tail.cdr)) {
//       tail
//       rv.push(fn(tail.cdr))
//     }

//     return list(...rv)
//   }
//   includes(value: any): boolean {
//     for (let p of this.each()) {
//       if (p === value) return true
//     }
//     if (this.isList) {
//       return false
//     }
//     if (isPair(this.tail.cdr))
//       return this.tail.cdr.includes(value)
//     return this.tail.cdr === value
//   }
//   get tail(): Pair {
//     let pair: Form = this,
//         tail: List = nil;
//     while (isPair(pair)) {
//       tail = pair
//       pair = pair.cdr
//     }
//     assert(isPair(tail), 'Invalid tail')
//     return tail
//   }
//   get isList() {
//     return !isNil(this.tail.cdr)
//   }
// }

// export class Binding {
//   constructor(
//     public expression: Form,
//     public scope: Scope,
//     public memoized = true,
//   ) {}

//   private _value!: any

//   force() {
//     if (this.scope.has(this._value) && this.memoized)
//       return this._value
//     this._value = evaluate(this.expression, this.scope)
//     return this._value
//   }

//   // This method is provided as a convenience so that a +Binding+ may be
//   // treated like any other expression during evaluation. All it does is
//   // return the result of calling <tt>force!</tt>.
//   eval(scope: Scope) {
//     this.force()
//   }

//   // We provide an equality method so that a bound +Identifier+ produced by
//   // expanding a macro can be matched against literal identifiers in another
//   // macro pattern.
//   equal(identifier: string) {
//     this.expression === identifier
//   }

//   innermost_binding(identifier: string) {
//     return this.scope
//   }

//   toString() {
//     return toString(this.expression)
//   }
// }
// export class Callable {
//   call(scope: Scope, exprs: List): any {}
// }
// export class Syntax extends Callable {
//   constructor(
//     public scope: Scope,
//     public formals: List,
//     public body: Function,
//     public name = 'lambda'
//   ) { super() }
//   call(scope: Scope, exprs: List) {
//     // return this.body.call(scope, params)

//     const params = isNil(exprs) ? exprs : exprs.map(pair => {
//       return pair
//     })
//     return this.body(scope, params)
//   }
//   toString() {
//     return `#<syntax:#${this.name}>`
//   }
//   static pattern_vars(pattern: Form, excluded: List = nil, results: string[] = []): string[] {
//     if (isNil(pattern))
//       return results
//     if (isIdentifier(pattern)) {
//       const name = pattern.description!
//       if ((isPair(excluded) && excluded.includes(name)) || RESERVED.includes(name))
//         return results
//       if (!results.includes(name))
//         results.push(name)
//     }
//     if (isPair(pattern)) {
//         const tail = pattern.tail as Pair
//         for (let cell of pattern.each()) {
//           this.pattern_vars(cell, excluded, results)
//         }
//         return this.pattern_vars(tail.cdr, excluded, results)
//     }
//     return results
//   }
// }
// export class Macro extends Callable {
//   constructor(
//     public scope: Scope,
//     public formals: List,
//     public body: Pair,
//     public name: string,
//   ) { super() }
//   call(scope: Scope, cells: List) {
//     const [rule, matches] = this.rule_for(cells, scope) as any; // see: SyntaxDef
//     return new Expansion(this.scope, scope, rule.cdr.car, matches)
//   }
//   private rule_for(cells: List, scope: Scope): [Pair, Matches] {
//     for (let rule of this.body.each()) {
//       const matches = this.rule_matches(scope, rule.car.cdr, cells)
//       if (matches instanceof Matches) {
//         return [rule, matches]
//       }
//     }
//     return error()
//   }
//   private rule_matches(scope: Scope, pattern: Form, input: Form, matches?: any, depth = 0): boolean | Matches | Nil {
//     matches = matches ?? new Matches(pattern, this.formals)

//     if (isList(pattern)) {
//       // If pattern is NULL, the input must also be NULL
//       if (isNil(pattern))
//         return (isNil(input)) ? matches : nil

//       // Fail if the pattern is a list and the input is not
//       assert(isPair(input))

//       // Iterate over the pattern, consuming input as we go
//       let pattern_pair = pattern as any
//       let input_pair = input as any

//       let skip = () => { pattern_pair = pattern_pair.cdr }

//       while (isPair(pattern_pair)) {
//         let token = pattern_pair.car

//         // Skip the current pattern token if it's an ellipsis
//         if (token === ellipsis) {
//           skip()
//           continue
//         }

//         // Increment the repetition depth if the next pattern token is an
//         // ellipsis, and inform the +Matches+ object that the pattern vars
//         // in the current pattern have hit a repetition boundary. Note we
//         // do not increment +depth+ itself since this would persist for the
//         // remaining tokens in the pattern after we get past the ellipsis.
//         let followed_by_ellipsis = ((<any>pattern_pair)?.cdr?.car == ellipsis ?? false)
//         let dx = followed_by_ellipsis ? 1 : 0

//         console.log((<any>pattern_pair)?.cdr?.car)
//         followed_by_ellipsis

//         if (followed_by_ellipsis)
//           matches.descend(Syntax.pattern_vars(token, this.formals),
//                           depth + dx)

//         // Set up a closure to consume input using the current pattern
//         // expression. Calls +rule_matches+ with the current scope,
//         // pattern, input, and +Matches+ object.
//         let consume = () => {
//           return isPair(input_pair) &&
//             this.rule_matches(scope, token, input_pair.car, matches, depth + dx)
//         }

//         // If the next pattern token is not an ellipsis, fail unless the
//         // pattern token matches the input token.
//         //
//         // If the next token is an ellipsis, consume input using the
//         // current pattern until the pattern no longer matches the current
//         // input.
//         //
//         let consumed = consume()

//         if (!(consumed || followed_by_ellipsis))
//           return nil

//         if (consumed) {
//           input_pair = input_pair.cdr
//         }

//         while (followed_by_ellipsis && consume()) {
//           input_pair = input_pair.cdr
//         }

//         skip()
//       }

//       // We're done iterating over the pattern, so the current pattern
//       // token will be NULL or some non-Cons object (if the pattern is an
//       // improper list). Fail unless the remaining input matches this
//       // object.
//       if (!this.rule_matches(scope, pattern_pair, input_pair, matches, depth))
//         return nil
//     }

//     // If the pattern is a formal keyword for the macro (a 'literal
//     // identifier' in the terms of the spec), return a boolean indicating
//     // whether the input is an identifier with the same binding, that is
//     // to say the two identifiers refer to the same location in memory (or
//     // both refer to no location). If it's a normal pattern variable, store
//     // the current input, whatever it is, in the +matches+.
//     else if (isIdentifier(pattern)) {

//       if (isPair(this.formals) && this.formals.includes(toString(pattern))) {
//         return (
//           (pattern === input) &&
//           this.scope.get(toString(pattern)) === scope.get(toString(input))
//           // (this.scope.innermost_binding(pattern) === scope.innermost_binding(input))
//         ) ?? nil
//       } else {
//         matches.put(pattern, input)
//       }
//     }

//     // If all above type checks on the pattern fail, assume the pattern is
//     // literal data and make sure the input matches.
//     else {
//       return pattern == input ? matches : nil
//     }

//     return matches
//   }
//   toString() {
//     return `(define-syntax ${this.name} (syntax-rules ${toString(this.formals)} ${toString(this.body.car)}))`
//   }
// }
// export class Expansion {
//   private hygienic: boolean;
//   public expression: Form;
//   constructor(
//     private lexical_scope: Scope,
//     private calling_scope: Scope,
//     template: List,
//     matches: Matches
//   ) {
//     this.hygienic = Expansion.runtime.hygienic
//     this.expression = this.expand(template, matches)
//   }

//   private expand(template: Form, matches: Matches, depth = 0, ignoring_ellipses = false): Form {
//     if (isList(template)) {
//       if (isNil(template))
//         return nil

//       if (template.car === ellipsis) {
//         return this.expand(cadr(template), matches, depth, true)
//       }

//       let result        : any = null,
//           last          : any = null,
//           repeater      : any = null,
//           template_pair : any = template;

//       const push = (value: Form) => {
//         let pair = new Pair(value)
//         pair.hosts(value)
//         result = result || pair
//         if (isPair(last))
//           last.cdr = pair
//         last = pair
//       }

//       while (isPair(template_pair)) {

//         const cell = template_pair.car

//         // Increment the repetition depth if the current subtemplate is
//         // followed by an ellipsis and we are not treating ellipses as
//         // literals
//         let followed_by_ellipsis = (isPair(template_pair.cdr) &&
//                                     cadr(template_pair) === ellipsis) &&
//                                     !ignoring_ellipses

//         const dx = followed_by_ellipsis ? 1 : 0

//         if (followed_by_ellipsis) { repeater = cell }

//         // Once we reach an ellipsis, expand the preceeding form the
//         // correct number of times depending on the +matches+
//         if (cell === ellipsis && !ignoring_ellipses) {
//           matches.expand(repeater, depth + 1, () => {
//             push(this.expand(repeater, matches, depth + dx))
//           })
//         }

//         // If the current subtemplate is not an ellipsis and is not
//         // followed by an ellipsis, expand it and push the result onto
//         // the output
//         else if (!followed_by_ellipsis) {
//           push(this.expand(cell, matches, depth + dx, ignoring_ellipses))
//         }

//         template_pair = template_pair.cdr
//       }

//       // Handle the tail of improper list templates
//       if (!isNil(last))
//         last.cdr = this.expand(template_pair, matches, depth, ignoring_ellipses)

//       return result
//     }
//     else if (isIdentifier(template)) {
//       // If the template is a pattern variable, return the current match
//       // for that variable. See +Matches+ to see how repeated patterns
//       // are handled.
//       if (matches.has(template))
//         return matches.get(template)


//       // Otherwise, if using unhygienic macros, return the template
//       // verbatim as a new symbol.
//       if (!this.hygienic)
//         return Symbol(template.description)

//       // If using hygienic macros: bind the identifier to the macro's
//       // lexical scope if it is defined there, otherwise rename it as
//       // appropriate to avoid clashes with variables in the calling scope.
//       return this.lexical_scope.has(template.description!)
//         ? new Binding(template, this.lexical_scope, false)
//         : this.rename(template.description!)

//     }
//     else {
//       return template
//     }
//   }
//   private rename(id: string) {
//     if (this.calling_scope.has(id)) {
//       let i = 1
//       while (this.calling_scope.has(`#${id}#${i}`)) {
//         i += 1
//       }
//       return Symbol.for(`#${id}#${i}`)
//     }
//     return id
//   }

//   toString() {
//     return toString(this.expression)
//   }

//   static runtime = { hygienic: true }
// }
// export class Matches {
//   public data: Record<string, Tree> = {}
//   constructor(
//     public pattern: Form,
//     public formals: List,
//   ) {
//     const names = Syntax.pattern_vars(pattern, formals)

//     names.forEach(name => {
//       this.data[name] = new Tree(name)
//     })
//   }
//   descend(names: string[], depth: number) {
//     Object.entries(this.data).forEach(([name, set]) => {
//       if (names.includes(name))
//         set.descend(depth)
//     })
//   }
//   put(name_: symbol, value: Form) {
//     const name = name_.description!
//     if (this.has(name_))
//       this.data[name].push(value)
//   }
//   has(name_: symbol) {
//     const name = name_.description!
//     return name in this.data
//   }
//   get(name_: symbol) {
//     const name = name_.description!
//     return this.data[name].read
//   }
//   expand(template: any, depth: number, block: Function): void {
//     const names = Syntax.pattern_vars(template)
//     range(0, this.size(names, depth))
//       .forEach(() => (block(), this.iterate(names, depth)))
//   }
//   size(names: any[], depth: number): number {
//     let sizes: any[] = []
//     Object.entries(this.data).forEach(([name, tree]) => {
//       if (names.includes(name)) { sizes.push(tree.size(depth)) }
//     })

//     // sizes = sizes.compact.uniq
//     let _sizes = [...new Set(sizes)]
//     if (_sizes.length == 1)
//       return _sizes[0]

//     throw new Error(
//       "Macro could not be expanded: mismatched repetition patterns")
//   }
//   iterate(names: any[], depth: number) {
//     Object.entries(this.data).forEach(([name, tree]) => {
//       if (names.includes(name))
//         tree.shift(depth)
//     })
//   }
//   toString() {
//     return `[${Matches.name}]`
//   }
// }
// export class Tree {
//   constructor(
//     public name: string,
//     public data: number[] = [],
//     public depth: number = 0,
//   ) {}
//   descend(depth: number) {
//     this.tail(depth-1).push([])
//     if (depth > this.depth)
//       this.depth = depth
//   }
//   push(value: Form) {
//     this.tail(this.depth).push(value)
//   }
//   get read() {
//     const a = this.current(this.depth);
//     const depth = this.indexes[this.depth];
//     return a[depth]
//   }
//   shift(depth: number) {
//     if (depth > this.depth) {
//       return
//     }

//     this._indexes[depth] += 1

//     const nextIdx = this._indexes[depth]
//     const current = this.current(depth);

//     if (nextIdx >= current.length) {
//       this._indexes[depth] = 0
//     }
//   }
//   size(depth: number): number {
//     return (depth > this.depth)
//       ? 0
//       : this.current(depth)?.length ?? 0
//   }
//   tail(depth: number) {
//     return range(0, depth)
//       .reduce(list => list[list.length-1], <any>this.data)
//   }
//   current(depth: number) {
//     return range(0, depth)
//       .map(i => this.indexes[i])
//       .reduce((list, i) => list[i], <any>this.data)
//   }

//   get indexes(): number[] {
//     if (!this._indexes) {
//       const xs = Array(this.depth).fill(0)
//       this._indexes = [0, ...xs]
//     }
//     return this._indexes
//   }

//   private  _indexes!: number[]
// }

// const isList = (obj: any): obj is Pair | symbol => (obj === nil || isPair(obj));
// const isPair = (obj: any): obj is Pair => obj instanceof Pair;
// const isNil = (obj: any): obj is symbol => obj === nil;
// const isIdentifier = (obj: any): obj is symbol =>  typeof obj === 'symbol';
// const isSymbol = (obj: any): obj is symbol => typeof obj === 'symbol';
// const isString = (obj: any): obj is string => typeof obj === 'string';
// const isNumber = (obj: any): obj is number => typeof obj === 'number';
// const isAtom = (obj: any): obj is Atom => (isSymbol(obj) || isNumber(obj) || isString(obj));

// const toString = (x: Form): string => {
//   if (isAtom(x)) {
//     if (isSymbol(x))
//       return x.description!
//     return String(x)
//   }
//   if (isList(x)) {
//     if (isNil(x.cdr)) { return `(${toString(x.car)})` }

//     let strings = []
//     for (const e of x.each()) {
//       strings.push(toString(e))
//     }
//     if (!isNil(x.tail.cdr))
//       strings.push('.', toString(x.tail.cdr))
//     return `(${strings.join(' ')})`
//   }
//   return x.toString()
// }

// //////////////////////////////////////////////////////////////////////
// //////////////////////////////////////////////////////////////////////
// //////////////////////////////////////////////////////////////////////

// const formals = nil
// const pattern = cons(Sym('and'), cons(Sym('expr1'), cons(Sym('expr2'), cons(ellipsis))))
// const template = cons(Sym('lambda'), cons(Sym('expr1'), cons(Sym('expr2'), cons(ellipsis))))

// const rules = cons(cons(pattern, cons(template)));
// console.log(toString(rules))

// const input = list(Sym('and'), Sym('1'), Sym('2'), Sym('3'), Sym('4'), Sym('hats')) as Pair
// console.log(toString(input))

// let scope1: Scope = new Map(),
//     scope2: Scope = new Map();

// scope1.set('hats', 'crazy')
// scope2.set('lambda', 'lambda-value')
// scope2.set('hats', 'party-time')

// const macro = new Macro(scope1, formals, rules, 'and')
// console.log(toString(macro));

// const expansion = macro.call(scope2, <Pair>input.cdr)

// console.log(toString(expansion));

