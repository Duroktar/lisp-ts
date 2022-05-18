
export type Atom = symbol | string | number;
export type List = Expr[];
export type Expr = List[] | List | Atom;
