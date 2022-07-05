
// @ts-nocheck
export class MutableString implements String {

  constructor(private __value: string) {
    this.__value = __value;
  }

   charAt() {
    return this.__value.charAt(...arguments);
  }

  charCodeAt() {
    return this.__value.charCodeAt(...arguments);
  }

  concat(): MutableString {
    this.__value = this.__value.concat(...arguments);
    return this;
  }

  endsWith() {
    return this.__value.endsWith(...arguments);
  }

  equal(other: any) {
    if (other instanceof MutableString || typeof other !== 'string')
      return this.toString() === other.toString()
    return false
  }

  includes() {
    return this.__value.includes(...arguments);
  }

  indexOf() {
    return this.__value.indexOf(...arguments);
  }

  lastIndexOf() {
    return this.__value.lastIndexOf(...arguments);
  }

  get length() {
    return this.__value.length
  }

  localeCompare() {
    return this.__value.localeCompare(...arguments);
  }

  match() {
    return this.__value.match(...arguments);
  }

  normalize(): MutableString {
    this.__value = this.__value.normalize(...arguments);
    return this;
  }

  repeat(): MutableString {
    this.__value = this.__value.repeat(...arguments);
    return this;
  }

  replace(): MutableString {
    this.__value = this.__value.replace(...arguments);
    return this;
  }

  replaceAll(...args: any[]): MutableString {
    this.__value = this.__value.replaceAll(...args);
    return this;
  }

  search() {
    return this.__value.search(...arguments);
  }

  slice(): MutableString {
    this.__value = this.__value.slice(...arguments);
    return this;
  }

  split() {
    return this.__value.split(...arguments);
  }

  startsWith() {
    return this.__value.startsWith(...arguments);
  }

  substr(): MutableString {
    this.__value = this.__value.substr(...arguments);
    return this;
  }

  substring(): MutableString {
    this.__value = this.__value.substring(...arguments);
    return this;
  }

  toLocaleLowerCase(): MutableString {
    this.__value = this.__value.toLocaleLowerCase(...arguments);
    return this;
  }

  toLocaleUpperCase(): MutableString {
    this.__value = this.__value.toLocaleUpperCase(...arguments);
    return this;
  }

  toLowerCase(): MutableString {
    this.__value = this.__value.toLowerCase(...arguments);
    return this;
  }

  toString() {
    return this.__value.toString();
  }

  toUpperCase(): MutableString {
    this.__value = this.__value.toUpperCase(...arguments);
    return this;
  }

  trim(): MutableString {
    this.__value = this.__value.trim(...arguments);
    return this;
  }

  trimLeft(): MutableString {
    this.__value = this.__value.trimLeft(...arguments);
    return this;
  }

  trimRight(): MutableString {
    this.__value = this.__value.trimRight(...arguments);
    return this;
  }

  valueOf() {
    return this.__value.valueOf();
  }

  toString() {
    return this.__value.toString();
  }

  [Symbol.iterator](): Iterator {
    return this.__value[Symbol.iterator]();
  }

  static equals(a: MutableString, b: MutableString): boolean {
    return a.__value === b.__value
  }
}

export function Str(string: string): MutableString {
  return new MutableString(string)
}
