
export function gcd(x: number, y: number) {
  x = Math.abs(x);
  y = Math.abs(y);
  while (y) {
    var t = y;
    y = x % y;
    x = t;
  }
  return x;
}
;

export function lcm(n1: number, n2: number) {
  //Find the gcd first
  let gcdr = gcd(n1, n2);

  //then calculate the lcm
  return Math.abs((n1 * n2) / gcdr);
}
