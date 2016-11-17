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
exception InvalidExpression of expression

(* Convenience bindings *)
fun eq x y = x = y
fun id x = x

val exists = List.exists
val filter = List.filter

fun dropWhile _ [] = []
  | dropWhile f (x::xs) = if f x then dropWhile f xs else x::xs
fun dropWhileR f = rev o (dropWhile f) o rev

(* Sorting the expression term order *)
fun lt_expr (Constant x) (Constant y) = x < y
  | lt_expr (Constant _) _ = true
  | lt_expr _ (Constant _) = false
  | lt_expr (Variable x) (Variable y) = x < y
  | lt_expr (Variable _) _ = true
  | lt_expr _ (Variable _) = false
  | lt_expr _ _ = true
fun gte_expr x y = not (lt_expr x y)

fun sort_expr [] = []
  | sort_expr (x::xs) =
    (sort_expr (filter (gte_expr x) xs))@[x]@(sort_expr (filter (lt_expr x) xs))

(* This can only be used with commutative operations due to sorting *)
fun operator s exp = Operator (s, List (sort_expr exp))

fun sort_oper (Operator (opr, List l)) = operator opr (sort_expr l)
  | sort_oper (Operator (opr, Pair l)) = operator opr (sort_expr l)

fun ++ (e1, e2) = operator "+" [e1, e2]
infix ++
fun -- (e1, e2) = Operator ("-", Pair [e1, e2]);
infix --
fun ** (e1, e2) = operator "*" [e1, e2];
infix **
fun // (e1, e2) = Operator ("/", Pair [e1, e2]);
infix //
fun %% (e1, e2) = Operator ("%", Pair [e1, e2]);
infix %%

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
exception InvalidOperator
fun str_to_operator opr =
  case opr of
       "+" => foldl op+ 0
     | "*" => foldl op* 1
     | "-" => foldl op- 0
     | _ => raise InvalidOperator

(* Evalue an expression given variables to use *)
fun eval _ (Constant x) = x
  | eval [] (Variable name) = raise InvalidVariable name
  | eval ((vname, value)::xs) (var as Variable name) =
      if vname = name then value else eval xs var
  | eval vars (e as Operator (opr, (Pair p))) =
      if length p = 2 andalso exists (eq opr) ["+", "*", "-"] then
        str_to_operator opr (map (eval vars) p)
      else if opr = "/" then
        let val (a::b::_) = p in (eval vars a) div (eval vars b) end
      else if opr = "%" then
        let val (a::b::_) = p in (eval vars a) mod (eval vars b) end
      else raise InvalidExpression e
  | eval vars (e as Operator (opr, (List l))) =
      if exists (eq opr) ["+", "*"] then
        str_to_operator opr (map (eval vars) l)
      else raise InvalidExpression e
  | eval _ e = raise InvalidExpression e

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
  | derivative e _ = raise InvalidExpression e

(* Naloga 5
 * Flatten an expression as a sum of products
 *)

(* Simplify the expression using the distribution property *)
fun distribute_two (Operator (s1, Pair l1)) (Operator (s2, Pair l2)) =
      distribute_two (Operator (s1, List l1)) (Operator (s2, List l2))
  | distribute_two (c as (Constant _)) (e as (Operator _)) = distribute_two e c
  | distribute_two (x as (Variable _)) (e as (Operator _)) = distribute_two e x
  | distribute_two (Operator (_, List l)) (c as Constant _) =
      map (operator "*") (cross_lists (listify l) (listify [c]))
  | distribute_two (Operator (_, List l)) (x as Variable _) =
      map (operator "*") (cross_lists (listify l) (listify [x]))
  | distribute_two (Operator (_, List l1)) (Operator (_, List l2)) =
      map (operator "*") (cross_lists (listify l1) (listify l2))

fun merge opr (c as Constant _) e = merge opr e c
  | merge opr (x as Variable _) e = merge opr e x
  (* (1 + 2) + a = 1 + 2 + a *)
  | merge "+" (Operator ("+", List e)) (c as Constant _) = operator "+" (c::e)
  | merge "+" (Operator ("+", List e)) (x as Variable _) = operator "+" (x::e)
  (* (a + b) + (c + d) = a + b + c + d *)
  | merge "+" (Operator (_, List e1)) (Operator (_, List e2)) =
      operator "+" (e1@e2)
  (* (1 * 2) * a = 1 * 2 * a *)
  | merge "*" (Operator ("*", List e)) (c as Constant _) = operator "*" (c::e)
  | merge "*" (Operator ("*", List e)) (v as Variable _) = operator "*" (v::e)
  (* 2 * (x + y) = 2x + 2y *)
  | merge "*" (e as (Operator ("+", _))) (c as Constant _) =
      operator "+" (distribute_two c e)
  (* 2 * (x + y) = 2x + 2y *)
  | merge "*" (e as (Operator ("+", _))) (x as Variable _) =
      operator "+" (distribute_two x e)
  (* (a + b) * (c + d) = ac + ad + bc + bd *)
  | merge opr (e1 as (Operator ("*", _))) (e2 as (Operator ("*", _))) =
      operator "+" (distribute_two e1 e2)

fun distribute_two_ke x (Operator (_, Pair [])) = [x]
  | distribute_two_ke (Operator (_, Pair [])) y = [y]
  | distribute_two_ke x (Operator (_, List [])) = [x]
  | distribute_two_ke (Operator (_, List [])) y = [y]
  | distribute_two_ke x y = distribute_two x y

fun distribute (Operator ("*", List l)) =
  Operator ("+", List (foldr (fn (x, xa) => (
    (distribute_two_ke x (Operator ("", List [Constant 5])))@xa
  )) [] l))

(* Naloga 6
 * Join similar nodes together
 *)

(* First function on constants, second on variables, third how to merge them,
 * fourth initial value
 *)
fun traverse c v t i (Constant x) = c x
  | traverse c v t i (Variable x) = v x
  | traverse c v t i (Operator (_, Pair p)) = foldl t i (map (traverse c v t i) p)
  | traverse c v t i (Operator (_, List p)) = foldl t i (map (traverse c v t i) p)
  | traverse _ _ _ _ e = raise InvalidExpression e

val allVars = (traverse (fn x => []) (fn x => [x]) (op @) []) o sort_oper
fun variablesMatch e1 e2 = allVars e1 = allVars e2

(* Find the first const in an expression or return 1 *)
fun firstConstOrOne (Operator (_, List l)) =
  let
    fun matchConst (Constant _) = true | matchConst _ = false
    val filtered = filter matchConst l
  in
    if not (null filtered) then
      hd filtered
    else (Constant 1)
  end

(* Add an together two expressions if they match e.g. x*x to x*x = 2*x*x *)
fun addToExpr (e, (Operator (_, List []))) = operator "+" [e]
  | addToExpr (e, (Operator (opr, List (x::xs)))) =
      if variablesMatch e x then
        let
          fun addToConstant (c as Constant _) [] = [c]
            | addToConstant (Constant c) ((Constant y)::ys) = Constant (c + y)::ys
            | addToConstant c (y::ys) = y::(addToConstant c ys)
          val (Operator (_, List xl)) = x
          val reduced = addToConstant (firstConstOrOne e) xl
        in
          operator opr [operator "*" reduced]
        end
      else
        let
          val Operator (_,  List reduced) = addToExpr (e, operator opr xs)
        in
          operator opr (x::reduced)
        end

fun joinSimilar (Operator (_, List e)) = foldl addToExpr (operator "+" []) e


(* Naloga 7
 * Remove empty nodes from the equation
 *)
fun isOne (Constant 1) = true | isOne _ = false
fun isZero (Constant 0) = true | isZero _ = false
val removeOnes = filter (not o isOne)
val removeZeros = filter (not o isZero)
fun reduceMult l = if exists isZero l then [] else l
(* Try to reduce a list of expressions into a single value, if possible *)
fun reduceList (Operator (_, List [])) = Constant 0
  | reduceList (Operator (_, List (x::[]))) = x
  | reduceList e = e

fun removeEmpty (c as Constant _) = c
  | removeEmpty (x as Variable _) = x
  | removeEmpty (Operator (opr, Pair p)) = removeEmpty (Operator (opr, List p))
  | removeEmpty (Operator ("*", List l)) =
      reduceList (operator "*" ((reduceMult o removeOnes) (map removeEmpty l)))
  | removeEmpty (Operator ("/", List l)) = operator "/" (dropWhileR isOne l)
  | removeEmpty (Operator ("+", List l)) =
      reduceList (operator "+" (removeZeros (map removeEmpty l)))
  | removeEmpty (Operator ("-", List l)) =
      reduceList (operator "-" (removeZeros (map removeEmpty l)))
  | removeEmpty e = raise InvalidExpression e


val test_m1 = merge "+"
  (Variable "x")
  (Operator ("+", List [Variable "c", Constant 2]))
val test_m2 = merge "*"
  (Variable "x")
  (Operator ("*", List [Variable "c", Constant 2]))
val test_m3 = merge "*"
  (Constant 2)
  (Operator ("+", List [Variable "x", Variable "y"]))

