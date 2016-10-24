datatype natural = NEXT of natural
                 | ZERO

datatype bstree = br of bstree * int * bstree
                | lf

datatype direction = L | R

exception PreviousOfZero

(* Tail recursion implementation of checking if list is sorted *)
fun sorted (lst : int list) =
  let
    fun sorted' (x1::x2::xs, _) = sorted' ((x2::xs), x1 < x2)
      | sorted' (_, valid) = valid
  in
    sorted' (lst, true)
  end

(* Check if a `natural` number is even *)
fun isEven ZERO = true
  | isEven (NEXT (ZERO)) = false
  | isEven (NEXT (NEXT m)) = isEven m

(* Get the previous value of a `natural` number, raise otherwise *)
fun previous ZERO = raise PreviousOfZero
  | previous (NEXT n) = n

(* Subtract two `natural` numbers from each other *)
fun subtract (n, ZERO) = n
  | subtract (NEXT n, NEXT m) = subtract (n, m)
  | subtract (ZERO, _) = raise PreviousOfZero

(* Tail recursion implementation of `any` *)
fun any (f, lst) =
  let
    fun any' _ _ true = true
      | any' _ [] a = a
      | any' f (x::xs) _ = any' f xs (f x)
  in
    any' f lst false
  end

fun map (_, []) = []
  | map (f, x::xs) = (f x)::(map (f, xs))

fun filter (_, []) = []
  | filter (f, (x::xs)) = (if f x then [] else [x])@(filter (f, xs))

fun fold (_, a, []) = a
  | fold (f, a, (x::xs)) = fold (f, (f (a, x)), xs)

