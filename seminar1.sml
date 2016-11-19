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
fun listify x = [x]
fun vectorize l = map listify l

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

fun sortOper (Operator (opr, List l)) = operator opr (sort_expr l)
  | sortOper (Operator (opr, Pair l)) = operator opr (sort_expr l)
  | sortOper (c as Constant _) = c
  | sortOper (x as Variable _) = x
  | sortOper e = raise InvalidExpression e

(* Naloga 1
 * Cartesian product that returns a list of tuples
 * e.g. cross([1,2], [3,4]) = [(1,3),(2,3),(1,4),(2,4)]
 *)
fun cross (a, b) = foldr (fn (x, xa) => (map (fn y => (x, y)) b)@xa) [] a

(* Naloga 2
 * Combinations of lists
 *)
(* Cartesian product on lists of lists *)
fun crossLists a b =
  foldr (fn (x, xa) => (foldr (fn (y, ya) => (x@y)::ya) [] b)@xa) [] a

(* Cartesian product on lists of lists. If one is empty, return other one *)
fun crossListsKE x [] = x
  | crossListsKE [] y = y
  | crossListsKE a b = crossLists a b

fun combinations ls = foldr (fn (l, la) => crossListsKE (vectorize l) la) [] ls

(* Naloga 3
 * Evaluate an expression
 *)
exception InvalidOperator
fun strToOperator opr =
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
        strToOperator opr (map (eval vars) p)
      else if opr = "/" then
        let val (a::b::_) = p in (eval vars a) div (eval vars b) end
      else if opr = "%" then
        let val (a::b::_) = p in (eval vars a) mod (eval vars b) end
      else raise InvalidExpression e
  | eval vars (e as Operator (opr, (List l))) =
      if exists (eq opr) ["+", "*"] then
        strToOperator opr (map (eval vars) l)
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
      map (operator "*") (crossLists (vectorize l) (vectorize [c]))
  | distribute_two (Operator (_, List l)) (x as Variable _) =
      map (operator "*") (crossLists (vectorize l) (vectorize [x]))
  | distribute_two (Operator (_, List l1)) (Operator (_, List l2)) =
      map (operator "*") (crossLists (vectorize l1) (vectorize l2))

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

val allVars = (traverse (fn _ => []) listify op@ []) o sortOper
fun variablesMatch e1 e2 = allVars e1 = allVars e2
(* Find the product of constants from the expression or return 1 if none found *)
val extractConstant = traverse id (fn _ => 1) op* 1
val onlyHasConstants = null o (traverse (fn _ => []) listify op@ [])
val onlyHasVars = null o (traverse listify (fn _ => []) op@ [])

(* Merge a constant into an expression list, given a function, if a variable
 * is given, append it to the end of the list since we can't merge them *)
fun addToExprList f (e, []) = [e]
  | addToExprList f ((Constant x), ((Constant y)::ys)) = (Constant (f (x, y)))::ys
  | addToExprList f (e, (y::ys)) = y::(addToExprList f (e, ys))

(* Reduce an expression into its simplest form, this only works on one level of
 * expressions, e.g. 2+2+x+3 = 7+x *)
fun reduceExpr (Operator (opr, (Pair l))) = reduceExpr (Operator (opr, List l))
  | reduceExpr (Operator ("*", (List l))) = operator "*" (foldl (addToExprList op* ) [] l)
  | reduceExpr (Operator ("+", (List l))) = operator "+" (foldl (addToExprList op+ ) [] l)
  | reduceExpr (Operator ("-", (List l))) = operator "-" (foldl (addToExprList op- ) [] l)
  | reduceExpr e = e

(* Wrap a single element in an operator e.g. x = (+ x) *)
fun wrapInOperator opr (c as (Constant _)) = operator opr [c]
  | wrapInOperator opr (x as (Variable _)) = operator opr [x]
  | wrapInOperator _ e = e

(* Add any expression to another expression, assume multiplication *)
fun addToExpr (e, a) =
  let
    fun addToExpr' (e, (Operator (_, (List [])))) = [e]
      | addToExpr' (e, (Operator (opr, (List ((x as (Operator (_, (List l))))::xs))))) =
          if variablesMatch e x then
            let
              (* add 5 3 = 15 vs add 2x 5x = 7x *)
              val combineFunc = if onlyHasConstants e then op* else op+
              val add = addToExprList combineFunc
              (* If the expression contains vars, figure out the k, if not, be c *)
              val c = Constant ((extractConstant o reduceExpr) e)
              (* If x constins only variables, stick a C1 to add properly *)
              val ls = if onlyHasVars x then add (Constant 1, l) else l
            in
              (operator "*" (add (c, ls)))::xs
            end
          else x::(addToExpr' (e, (Operator (opr, List xs))))
  in
    operator "+" (addToExpr' (e, a))
  end

fun joinSimilar (Operator (opr, (Pair l))) = joinSimilar (Operator (opr, List l))
  | joinSimilar (Operator ("+", (List l))) =
      foldl addToExpr (operator "+" []) (map (reduceExpr o (wrapInOperator "*")) l)
  | joinSimilar e = raise InvalidExpression e

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

