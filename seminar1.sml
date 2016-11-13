(*
 * FP Seminar 1
 *)
Control.Print.printDepth := 100;


datatype expression = Constant of int
                    | Variable of string
                    | Operator of string * expression
                    | Pair of expression list
                    | List of expression list

exception InvalidVariable of string
exception InvalidExpression

(* Convenience bindings *)
fun div' (x, y) = x div y
fun mod' (x, y) = x mod y
fun eq x y = x = y
val exists = List.exists

(* Naloga 1
 * Cartesian product that returns a list of tuples
 *)
fun cross (a, b) = foldr (fn (x, xa) => (map (fn y => (x, y)) b)@xa) [] a

(* Naloga 2
 * Combinations of lists
 *)

(* Take a list and wrap each element inside another list *)
fun listify l = map (fn x => [x]) l

(* Cartesian product on lists of lists *)
fun cross_lists a b =
  foldr (fn (x, xa) => (foldr (fn (y, ya) => (x@y)::ya) [] b)@xa) [] a

(* Cartesian product on lists of lists. In case of one empty list, return other
 * non-empty one
 *)
fun cross_lists_ke x [] = x
  | cross_lists_ke [] y = y
  | cross_lists_ke a b = cross_lists a b

fun combinations ls = foldr (fn (l, la) => cross_lists_ke (listify l) la) [] ls

(* Naloga 3
 * Evaluate an expression
 *)
fun str_to_operator opr =
  case opr of
       "+" => foldl op+ 0
     | "*" => foldl op* 1
     | "-" => foldl op- 0
     | "/" => foldl div' 1
     | "%" => foldl mod' 1
     | _ => raise InvalidExpression

(* Evalue an expression given variables to use *)
fun eval _ (Constant x) = x
  | eval [] (Variable name) = raise InvalidVariable name
  | eval ((vname, value)::xs) (var as Variable name) =
      if vname = name then value else eval xs var
  | eval vars (Operator (opr, (Pair p))) =
      if length p = 2 then
        str_to_operator opr (map (eval vars) p)
      else raise InvalidExpression
  | eval vars (Operator (opr, (List l))) =
      if exists (eq opr) ["+", "*"] then
        str_to_operator opr (map (eval vars) l)
      else raise InvalidExpression
  | eval _ _ = raise InvalidExpression

(* Naloga 4
 * Calculate the derivative of an expression
 *)
fun derivative (Constant _) _ = Constant 0
  | derivative (Variable x) d = Constant (if x = d then 1 else 0)
  | derivative (Operator ("+", (Pair (x::y::_)))) d =
      Operator ("+", Pair [derivative x d, derivative y d])
  | derivative (Operator ("-", (Pair (x::y::_)))) d =
      Operator ("-", Pair [derivative x d, derivative y d])
  | derivative (Operator ("*", (Pair (x::y::_)))) d =
      Operator ("+", Pair [
        Operator ("*", Pair [derivative x d, y]),
        Operator ("*", Pair [derivative y d, x])
      ])
  | derivative (Operator ("/", (Pair (x::y::_)))) d =
      Operator ("/", Pair [
        Operator ("-", Pair [
          Operator ("*", Pair [derivative x d, y]),
          Operator ("*", Pair [derivative y d, x])
        ]),
        Operator ("*", Pair [y, y])
      ])
  | derivative _ _ = raise InvalidExpression

(* Naloga 5
 * Flatten an expression as a sum of products
 *)
fun flatten (c as Constant _) = c
  | flatten (x as Variable _) = x
  | flatten (Operator (opr, (Pair expr))) = flatten (Operator (opr, List expr))
  | flatten (Operator ("+", l as (List expr))) = l
  | flatten (Operator ("-", l as (List expr))) = l
  (*| flatten (Operator ("*", (List expr))) =
      List (foldl (fn (x, a) => map List (cross_lists (listify x) a)@a) []
      expr)*)
  | flatten _ = raise InvalidExpression

(* Split an expression into unary expressions
 * Example:  (x - 2 - 3) -> (+ (+x) (-2) (-3))
 *)
fun split_expr (Operator (_, List [])) = []
  | split_expr (Operator (_, List (x::[]))) = [Operator ("+", List [x])]
  | split_expr (Operator (s, (List e))) =
      (Operator ("+", List [hd e]))::
      (map (fn x => (Operator (s, List [x]))) (tl e))

fun sign "+" "-" = "-" | sign "-" "+" = "-" | sign _ _ = "+"

fun distribute (e1 : expression) (e2 : expression) : expression list =
  let
    val split = listify o split_expr
    (* Merge expressions produced by cross_lists.
     * Will nicely merge expressions with "+", but will still work on "-"
     *)
    fun m ((e1 as (Operator (s1, List l1)))::(e2 as (Operator (s2, List l2)))::[]) =
      if sign s1 s2 = "-" then
        Operator ("*", List [e1, e2])
      else Operator ("*", List (l1@l2))
  in
    map m (cross_lists (split e1) (split e2))
  end

fun distribute_ke x (Operator (_, List [])) = [x]
  | distribute_ke (Operator (_, List [])) y = [y]
  | distribute_ke x y = distribute x y

fun distribute' (Operator ("*", List l)) =
  foldr (fn (x, xa) => (foldr (fn (y, ya) => (distribute_ke x y)@ya) [] xa)@xa) [] l

val test = distribute' (Operator ("*", List [
  (Operator ("+", List [Variable "a", Variable "b"])),
  (Operator ("+", List [Constant 2, Constant 4])),
  (Operator ("+", List [Variable "x", Variable "y"]))
]));

