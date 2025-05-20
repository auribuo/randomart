module Rational = struct
  type rat = int * int

  let rat_of_int (i : int) = (i, 1)

  let float_of_rat x =
    let a, b = x in
    float_of_int a /. float_of_int b

  let invert (x : rat) =
    let a, b = x in
    (b, a)

  let abs x =
    let a, b = x in
    (abs a, abs b)

  let sign x =
    let a, b = x in
    let asign = if a < 0 then -1 else 1 in
    let bsign = if b < 0 then -1 else 1 in
    asign * bsign

  let gcd x =
    let a, b = x in
    let rec gcd a b = if b = 0 then a else gcd b (a mod b) in
    gcd a b

  let simplify x =
    let asign = sign x in
    let a, b = abs x in
    let d = gcd (a, b) in
    (a / d * asign, b / d)

  let neg x =
    let a, b = simplify x in
    simplify (-a, b)

  let add x y =
    let n1, d1 = simplify x in
    let n2, d2 = simplify y in
    simplify ((n1 * d2) + (n2 * d1), d1 * d2)

  let sub x y =
    let n1, d1 = simplify x in
    let n2, d2 = simplify y in
    simplify ((n1 * d2) - (n2 * d1), d1 * d2)

  let mul x y =
    let n1, d1 = simplify x in
    let n2, d2 = simplify y in
    let gcd_n1 = gcd (n1, d2) in
    let gcd_n2 = gcd (n2, d1) in
    let n1' = n1 / gcd_n1 in
    let d2' = d2 / gcd_n1 in
    let n2' = n2 / gcd_n2 in
    let d1' = d1 / gcd_n2 in
    simplify (n1' * n2', d1' * d2')

  let div x y = mul x (invert y)
end
