(*
 * NALOGA 5
 *)

Control.Print.printDepth := 100;

datatype expression =  Constant of int |
        Variable of string |
        Operator of string * expression |
        Pair of expression list |
        List of expression list

fun gcd a 0 = a | gcd a b = gcd b (a mod b)
fun lcm a b = (a * b) div (gcd a b)

fun operator opr l = Operator (opr, List l)
(* Fraction construction shortcut *)
fun fract n d = Operator ("/", Pair [n, d])
(* Take a fraction and simlify it *)
fun simplify (Operator ("/", Pair ((Constant n)::(Constant d)::_))) =
      let
        val x = gcd n d
        val s_n = n div x
        val s_d = d div x
      in
        if s_d <> 1 then
          Operator ("/", Pair [Constant s_n, Constant s_d])
        else Constant s_n
      end
  | simplify (expr as Operator ("/", Pair (n::(Constant d)::_))) =
      if d = 1 then n else expr

(* NALOGA 1: Fraction operator for ints *)
fun // (n, d) = simplify (fract (Constant n) (Constant d))
infix //

(* Fraction operator for constants *)
fun fracts (Constant n) (Constant d) = n // d
  | fracts e1 e2 = simplify (fract e1 e2)
(* Covert an expression into a fraction (usually means divide by 1) *)
fun fractionify (f as Operator ("/", _)) = f
  | fractionify x = fract x (Constant 1)

(* NALOGA 2: Multiply two simple fractions *)
fun multiply (x, (Constant 1)) = x
  | multiply ((Constant 1), x) = x
  | multiply ((Constant x), (Constant y)) = Constant (x * y)
  (* Variables with constants *)
  | multiply ((x as Variable _), (c as Constant _)) = operator "*" [c, x]
  | multiply ((c as Constant _), (x as Variable _)) = operator "*" [c, x]
  | multiply ((x as Variable _), (y as Variable _)) = operator "*" [x, y]
  (* Operator pairs *)
  | multiply ((Operator ("*", Pair l1)), (Operator ("*", Pair l2))) = operator "*" (l1@l2)
  | multiply ((Operator ("*", List l1)), (Operator ("*", List l2))) = operator "*" (l1@l2)
  | multiply ((e1 as (Operator ("+", _))), e2) = operator "*" [e1, e2]
  | multiply (e1, (e2 as (Operator ("+", _)))) = operator "*" [e1, e2]
  (* Fractions *)
  | multiply ((Operator ("/", (Pair (an::ad::_)))),
              (Operator ("/", (Pair (bn::bd::_))))) =
      fract (multiply (an, bn)) (multiply (ad, bd))
  (* Anything else *)
  | multiply (e1, e2) = multiply (fractionify e1, fractionify e2)

(* NALOGA 3: Add two simple fractions *)
fun add2 ((Constant 0), x) = x
  | add2 (x, (Constant 0)) = x
  | add2 ((Constant x), (Constant y)) = Constant (x + y)
  (* Variables with constants *)
  | add2 ((c as Constant _), (x as Variable _)) = operator "+" [c, x]
  | add2 ((x as Variable _), (c as Constant _)) = operator "+" [c, x]
  | add2 ((x as Variable _), (y as Variable _)) = operator "+" [x, y]
  (* Operator pairs *)
  | add2 ((Operator ("+", Pair l1)), (Operator ("+", Pair l2))) = operator "+" (l1@l2)
  | add2 ((Operator ("+", List l1)), (Operator ("+", List l2))) = operator "+" (l1@l2)
  | add2 ((e1 as (Operator ("*", _))), e2) = operator "+" [e1, e2]
  | add2 (e1, (e2 as (Operator ("*", _)))) = operator "+" [e1, e2]
  (* Fractions *)
  | add2 ((Operator ("/", Pair (an::(Constant ad)::_))),
          (Operator ("/", Pair (bn::(Constant bd)::_)))) =
      let
        val cm = lcm ad bd
        val mult1 = multiply (an, Constant ((cm div ad)))
        val mult2 = multiply (bn, Constant ((cm div bd)))
      in
        fracts (add2 (mult1, mult2)) (Constant cm)
      end
  (* Anything else *)
  | add2 (e1, e2) = add2 (fractionify e1, fractionify e2)

(* NALOGA 4: Add together a list of fractions *)
fun add3 ls = foldl add2 (Constant 0) ls

