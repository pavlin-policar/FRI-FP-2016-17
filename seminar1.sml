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

fun gcd (a, 0) = a
  | gcd (a, b) = gcd (b, (a mod b))

val exists = List.exists
val filter = List.filter

(* Sorting the expression term order *)
fun ltExpr (Constant x) (Constant y) = x < y
  | ltExpr (Constant _) _ = true
  | ltExpr _ (Constant _) = false
  | ltExpr (Variable x) (Variable y) = x > y
  | ltExpr (Variable _) _ = true
  | ltExpr _ (Variable _) = false
  | ltExpr _ _ = true
fun gteExpr x y = not (ltExpr x y)

fun sortExpr [] = []
  | sortExpr (x::xs) = (sortExpr (filter (gteExpr x) xs))@[x]@(sortExpr (filter (ltExpr x) xs))

(* This can only be used with commutative operations due to sorting *)
fun operator s exp = Operator (s, List (sortExpr exp))

fun sortOper (Operator (opr, List l)) = operator opr (sortExpr l)
  | sortOper (Operator (opr, Pair l)) = operator opr (sortExpr l)
  | sortOper (c as Constant _) = c
  | sortOper (x as Variable _) = x
  | sortOper e = raise InvalidExpression e

(* Convert a List into a Pair chain *)
fun pairify (Operator (opr, List (x1::x2::[]))) = Operator (opr, Pair [x1, x2])
  | pairify (Operator (opr, List (x1::x2::xs))) =
      Operator (opr, Pair [x1, pairify (Operator (opr, List (x2::xs)))])
  | pairify e = raise InvalidExpression e

(* Wrap a single element in an operator e.g. x = (+ x) *)
fun wrapInOperator opr (c as Constant _) = operator opr [c]
  | wrapInOperator opr (x as Variable _) = operator opr [x]
  | wrapInOperator _ e = e

(* If the operator contains a single operand, we can skip the operator *)
fun bringOutSingle (Operator (s, Pair p)) = bringOutSingle (Operator (s, List p))
  | bringOutSingle (Operator (s, List (x::[]))) = x
  | bringOutSingle e = e

(* Naloga 1
 * Cartesian product that returns a list of tuples
 * e.g. cross([1,2], [3,4]) = [(1,3),(2,3),(1,4),(2,4)]
 *)
fun cross (a, b) = foldr (fn (x, xa) => (map (fn y => (x, y)) b)@xa) [] a

(* Naloga 2
 * Combinations of lists
 *)
(* Cartesian product on lists of lists *)
fun crossLists a b = foldr (fn (x, xa) => (foldr (fn (y, ya) => (x@y)::ya) [] b)@xa) [] a

(* Cartesian product on lists of lists. If one is empty, return other one *)
fun crossListsKE x [] = x
  | crossListsKE [] y = y
  | crossListsKE a b = crossLists a b

fun combinations ls = foldr (fn (l, la) => crossListsKE (vectorize l) la) [] ls

(* Naloga 3
 * Evaluate an expression
 *)
exception InvalidOperator of string
fun strToOperator opr =
  case opr of
       "+" => foldl op+ 0
     | "*" => foldl op* 1
     | s => raise InvalidOperator s

(* Evalue an expression given variables to use *)
fun eval _ (Constant x) = x
  | eval [] (Variable name) = raise InvalidVariable name
  | eval ((vname, value)::xs) (var as Variable name) =
      if vname = name then value else eval xs var
  | eval vars (e as Operator (opr, Pair p)) =
      if length p = 2 andalso exists (eq opr) ["+", "*"] then
        strToOperator opr (map (eval vars) p)
      else if opr = "-" then
        let val (a::b::_) = p in (eval vars a) - (eval vars b) end
      else if opr = "/" then
        let
          val (a::b::_) = p
          val denominator = eval vars b
        in
          if denominator <> 0 then
            (eval vars a) div denominator
          else raise InvalidExpression e
        end
      else if opr = "%" then
        let
          val (a::b::_) = p
          val denominator = eval vars b
        in
          if denominator <> 0 then
            (eval vars a) mod denominator
          else raise InvalidExpression e
        end
      else if opr = "gcd" then
        eval vars (Operator ("gcd", List p))
      else raise InvalidExpression e
  | eval vars (e as Operator (opr, List l)) =
      if exists (eq opr) ["+", "*"] then
        strToOperator opr (map (eval vars) l)
      else if opr = "gcd" then
        let
          val (x::xs) = l
        in
          foldl (fn (n, a) => gcd ((eval vars n), a)) (eval vars x) xs
        end
      else raise InvalidExpression e
  | eval _ e = raise InvalidExpression e

(* Naloga 4
 * Calculate the derivative of an expression
 *)
fun derivative (Constant _) _ = Constant 0
  | derivative (Variable x) d = Constant (if x = d then 1 else 0)
  | derivative (e as Operator (opr, l as List _)) d = derivative (pairify e) d
  | derivative (Operator ("+", Pair (x::y::_))) d =
      Operator ("+", Pair [derivative x d, derivative y d])
  | derivative (Operator ("-", Pair (x::y::_))) d =
      Operator ("-", Pair [derivative x d, derivative y d])
  | derivative (Operator ("*", Pair (x::y::_))) d =
      Operator ("+", Pair [
        Operator ("*", Pair [derivative x d, y]),
        Operator ("*", Pair [derivative y d, x])
      ])
  | derivative (Operator ("/", Pair (x::y::_))) d =
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
fun flattenProduct [] = []
  | flattenProduct ((c as Constant _)::xs) = c::(flattenProduct xs)
  | flattenProduct ((x as Variable _)::xs) = x::(flattenProduct xs)
  | flattenProduct ((Operator ("*", List l))::xs) = (flattenProduct l)@(flattenProduct xs)
  | flattenProduct l = l

fun addNegOne (c as Constant _) = Operator ("*", List [Constant ~1, c])
  | addNegOne (x as Variable _) = Operator ("*", List [Constant ~1, x])
  | addNegOne (Operator (opr, Pair p)) = addNegOne (Operator (opr, List p))
  | addNegOne (Operator (opr, List l)) = Operator (opr, List ((Constant ~1)::l))
  | addNegOne e = raise InvalidExpression e

fun isNegOne (Constant ~1) = true
  | isNegOne _ = false

fun flatten' (c as Constant _) = [c]
  | flatten' (x as Variable _) = [x]
  | flatten' (Operator (opr, Pair l)) = flatten' (Operator (opr, List l))
  | flatten' (Operator ("+", List l)) = foldl op@ [] (map flatten' l)
  | flatten' (Operator ("-", List (x::xs))) =
      filter (not o isNegOne) ((flatten' x)@(foldl op@ [] (map (flatten' o addNegOne) xs)))
  | flatten' (e as Operator ("*", List l)) =
      (map (operator "*") (map flattenProduct (combinations (map flatten' l))))
  | flatten' e = raise InvalidExpression e

fun flatten (Operator ("-", Pair p)) = flatten (Operator ("-", List p))
  | flatten (Operator ("-", List [])) = Constant 0
  | flatten (Operator ("-", List (x::[]))) = x
  | flatten e = operator "+" ((map (wrapInOperator "*") o flatten') e)

(* Naloga 6
 * Join similar nodes together
 *)

(* Transform constants -> transform variables -> merge transformed -> initial *)
fun traverse c v t i (Constant x) = c x
  | traverse c v t i (Variable x) = v x
  | traverse c v t i (Operator (_, Pair p)) = foldl t i (map (traverse c v t i) p)
  | traverse c v t i (Operator (_, List p)) = foldl t i (map (traverse c v t i) p)
  | traverse _ _ _ _ e = raise InvalidExpression e

val allVars = (traverse (fn _ => []) listify op@ []) o sortOper
fun variablesMatch e1 e2 = allVars e1 = allVars e2
val prodConstants = traverse id (fn _ => 1) op* 1

(* Reduce a single multiplication expression by multiplying constants *)
fun reduceSingle (e as Operator ("*", _)) =
      operator "*" ((Constant (prodConstants e))::(map Variable (allVars e)))
  | reduceSingle e = raise InvalidExpression e

(* Add an expression into an expression of sum of products *)
fun addToExpr (e, (Operator ("+", List []))) = [reduceSingle e]
  | addToExpr (e, (Operator ("+", List (x::xs)))) =
      if variablesMatch e x then
        let
          val sumConstants = op+ (prodConstants e, prodConstants x)
          val c = if sumConstants = 1 then [] else [Constant sumConstants]
        in
          (Operator ("*", List (c@(map Variable (allVars e)))))::xs
        end
      else x::(addToExpr (e, Operator ("+", List xs)))
  | addToExpr (e1, e2) = raise InvalidExpression (operator "?" [e1, e2])

fun joinSimilar (Operator ("+", List l)) =
      foldl (operator "+" o addToExpr) (operator "+" []) (map (wrapInOperator "*") l)
  | joinSimilar e = e

(* Naloga 7
 * Remove empty nodes from the equation
 *)
fun isOne (Constant 1) = true | isOne _ = false
fun isZero (Constant 0) = true | isZero _ = false
val removeOnes = filter (not o isOne)
val removeZeros = filter (not o isZero)
(* Try to reduce a list of expressions into a single value, if possible *)
fun bringOutDef def (Operator (_, List [])) = Constant def
  | bringOutDef _ (Operator (_, List (x::[]))) = x
  | bringOutDef _ e = e

(* Try to evaluate a product to 0 *)
fun reduceMultExpr (Operator ("*", List l)) = if exists isZero l then Constant 0 else operator "*" l
  | reduceMultExpr e = raise InvalidExpression e

val onlyConstants = traverse (fn _ => true) (fn _ => false) (fn (x, y) => x andalso y) true

fun addConstants (Constant c, Constant a) = Constant (c + a)

fun removeEmpty (c as Constant _) = c
  | removeEmpty (x as Variable _) = x
  | removeEmpty (Operator (opr, Pair p)) = removeEmpty (Operator (opr, List p))
  | removeEmpty (Operator ("+", List (x::[]))) = x
  | removeEmpty (Operator ("+", List l)) =
    let
      val removed = bringOutDef 0 (operator "+" (removeZeros (map removeEmpty l)))
      val (Operator (_, List l)) = removed
    in
      if onlyConstants removed then
        foldl addConstants (Constant 0) l
      else removed
    end
  | removeEmpty (Operator ("-", List (x::xs))) =
      bringOutDef 0 (Operator ("-", List (x::(removeZeros (map removeEmpty xs)))))
  | removeEmpty (Operator ("*", List l)) =
      bringOutDef 1 ((reduceMultExpr o operator "*") (removeOnes (map removeEmpty l)))
  | removeEmpty (Operator ("/", List l)) = bringOutDef 1 (Operator ("/", List (removeOnes l)))
  | removeEmpty e = raise InvalidExpression e

(* Naloga 8
 * Simplify an expression to the smallest number of operantors
 *)
fun numOperators (Constant _) = 0
  | numOperators (Variable _) = 0
  | numOperators (Operator (opr, Pair p)) = numOperators (Operator (opr, List p))
  | numOperators (Operator (_, List l)) = 1 + (foldl op+ 0 (map numOperators l))
  | numOperators _ = 0

fun simplify (c as Constant _) = c
  | simplify (x as Variable _) = x
  | simplify (e as Operator ("*", _)) = simplify (Operator ("+", List [e]))
  | simplify e =
  let
    fun simplify' (Operator (opr, List l)) = operator opr (map bringOutSingle l)
      | simplify' e = e
    val simplified = (bringOutSingle o simplify' o joinSimilar o removeEmpty o flatten) e
    val shortened = bringOutSingle e
  in
    if numOperators simplified < numOperators shortened then
      simplified
    else shortened
  end

(* Naloga 9
 * Match an expression with a given pattern
 *)
datatype pattern = ConstantP of int
                 | ListP of pattern list
                 | OperatorP of string * pattern
                 | PairP of pattern list
                 | VariableP of string
                 | Wildcard

fun interleave xs x [] = [xs@[x]]
  | interleave xs x (y::ys) = (xs@(x::y::ys))::(interleave (xs@[y]) x ys)

fun permutations [] = [[]]
  | permutations (x::xs) = foldr op@ [] (map (interleave [] x) (permutations xs))

fun zipTwo x y = ListPair.zip(x, y)

(* Take a list of (exp, patt) and check that everything matches *)
fun checkMatch [] = SOME []
  | checkMatch ((x, p)::xs) =
      let
        val (thisMatch, restMatches) = (match (x, p), checkMatch xs)
      in
        if isSome thisMatch andalso isSome restMatches then
          SOME ((valOf thisMatch)@(valOf restMatches))
        else NONE
      end
(* Check a list of matches and return the first match *)
and extractMatch [] = NONE
  | extractMatch (x::xs) = if isSome x then x else extractMatch xs
and match (_, Wildcard) = SOME []
  | match (x, VariableP p) = SOME [(p, x)]
  | match (Constant x, ConstantP p) = if x = p then SOME [] else NONE
  | match (Pair xs, PairP ps) =
      if length xs = 2 andalso length ps = 2 then
        checkMatch (zipTwo xs ps)
      else NONE
  | match (List xs, ListP ps) =
      if length xs = length ps then
        extractMatch (map checkMatch (map (zipTwo xs) (permutations ps)))
      else NONE
  | match (Operator (s, l), OperatorP (ps, pl)) = if s = ps then match (l, pl) else NONE
  | match (_, _) = NONE

