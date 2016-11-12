(*
 * FP Seminar 1
 *)

datatype expression = Constant of int
                    | Variable of string
                    | Operator of string * expression
                    | Pair of expression list
                    | List of expression list

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

