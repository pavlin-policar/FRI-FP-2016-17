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
fun multiply ((Constant x), (Constant y)) = Constant (x * y)
  | multiply ((Operator ("/", (Pair (an::ad::_)))),
              (Operator ("/", (Pair (bn::bd::_))))) =
      fracts (multiply (an, bn)) (multiply (ad, bd))

(* NALOGA 3: Add two simple fractions *)
fun add ((Constant x), (Constant y)) = Constant (x + y)
  | add ((Operator ("/", (Pair (an::(Constant ad)::_)))),
         (Operator ("/", (Pair (bn::(Constant bd)::_))))) =
      let
        val cm = lcm ad bd
        val mult1 = multiply (an, Constant (cm div ad))
        val mult2 = multiply (bn, Constant (cm div bd))
      in
        fracts (add (mult1, mult2)) (Constant cm)
      end

(* NALOGA 4: Add two regular fractions with variables *)
fun multiply' (Constant x) (Constant y) = Constant (x * y)
  | multiply' (x as Variable _) (y as Variable _) = operator "*" [x, y]
  | multiply' (x as Constant _) (y as Variable _) = operator "*" [x, y]
  | multiply' (y as Variable _) (x as Constant _) = operator "*" [x, y]
  (* constant with plus operator *)
  | multiply' (x as Constant _) (e as (Operator ("+", _))) =
      operator "*" [x, e]
  | multiply' (e as (Operator ("+", _))) (x as Constant _) =
      operator "*" [x, e]
  (* variable with plus operator *)
  | multiply' (x as Variable _) (e as (Operator ("+", _))) =
      operator "*" [x, e]
  | multiply' (e as (Operator ("+", _))) (x as Variable _) =
      operator "*" [x, e]
  (* Multiply with multiplication expression *)
  | multiply' (x as Constant _) (Operator ("*", (List l))) = operator "*" (x::l)
  | multiply' (Operator ("*", (List l))) (x as Constant _) = operator "*" (x::l)
  | multiply' (x as Constant _) (Operator ("*", (Pair l))) = operator "*" (x::l)
  | multiply' (Operator ("*", (Pair l))) (x as Constant _) = operator "*" (x::l)
  | multiply' (e1 as (Operator _)) (e2 as (Operator _)) = operator "*" [e1, e2]

fun add2 ((Constant x), (Constant y)) = Constant (x + y)
  | add2 ((x as Variable _), (y as Variable _)) = operator "+" [x, y]
  (* Add with plus expression *)
  | add2 ((x as Constant _), (Operator ("+", (List l)))) = operator "+" (x::l)
  | add2 ((Operator ("+", (List l))), (x as Constant _)) = operator "+" (x::l)
  | add2 ((x as Constant _), (Operator ("+", (Pair l)))) = operator "+" (x::l)
  | add2 ((Operator ("+", (Pair l))), (x as Constant _)) = operator "+" (x::l)
  (* Add two plus expressions *)
  | add2 ((Operator ("+", (List l1))), (Operator ("+", (List l2)))) =
    operator "+" (l1@l2)
  | add2 ((Operator ("+", (Pair l1))), (Operator ("+", (Pair l2)))) =
    operator "+" (l1@l2)
  (* Fractions *)
  | add2 ((Operator ("/", (Pair (an::(Constant ad)::_)))),
          (Operator ("/", (Pair (bn::(Constant bd)::_))))) =
      let
        val cm = lcm ad bd
        val mult1 = multiply' an (Constant (cm div ad))
        val mult2 = multiply' bn (Constant (cm div bd))
      in
        fracts (add2 (mult1, mult2)) (Constant cm)
      end
  | add2 ((e1 as (Operator ("/", _))), (x as (Constant _))) = add2 (e1, fractionify x)
  | add2 ((x as (Constant _)), (e1 as (Operator ("/", _)))) = add2 (e1, fractionify x)
  (* Add other expressions *)
  | add2 ((e1 as (Operator _)), (e2 as (Operator _))) = operator "+" [e1, e2]
  (* Add with product *)
  | add2 ((x as (Constant _)), (e as (Operator _))) = operator "+" [x, e]
  | add2 ((e as (Operator _)), (x as (Constant _))) = operator "+" [x, e]

(* NALOGA 5: Add together a list of fractions *)
fun add3 ls = foldl add2 (Constant 0) ls

