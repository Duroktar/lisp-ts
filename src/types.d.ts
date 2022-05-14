export type Predicate = (...args: any[]) => boolean

export type Position = {
  line: number;
  col: number;
  cursor: number;
};
