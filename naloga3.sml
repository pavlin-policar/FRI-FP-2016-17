datatype natural = NEXT of natural | ZERO
datatype bstree = br of bstree * int * bstree | lf
datatype direction = L | R

exception PreviousOfZero

(* Tail recursion implementation of checking if list is sorted *)
fun sorted (x1::x2::xs) = x1 <= x2 andalso sorted (x2::xs)
  | sorted _ = true

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
fun any (_, []) = false
  | any (f, x::xs) = f x orelse any (f, xs)

fun map (_, []) = []
  | map (f, x::xs) = (f x)::(map (f, xs))

fun filter (_, []) = []
  | filter (f, (x::xs)) = (if f x then [x] else [])@(filter (f, xs))

fun fold (_, a, []) = a
  | fold (f, a, (x::xs)) = fold (f, (f (a, x)), xs)

(* AVL Trees - rotate branch *)
fun rotate (lf, _) = lf
  | rotate (br (t1, z, br (t2, y, x)), L) = br (br (t1, z, t2), y, x)
  | rotate (br (br (x, y, t3), z, t4), R) = br (x, y, br (t3, z, t4))
  | rotate (t, _) = t

(* AVL Trees - insert *)
fun max x y = if x > y then x else y

(* Get the depth of a tree node *)
fun depth lf = 0
  | depth (br (left, _, right)) = max (depth left) (depth right) + 1
=
(* Utility method for testing - not really used anywhere at inside AVL *)
fun diff lf = 0
  | diff (br (l, v, r)) = (depth l) - (depth r)

fun avl (lf, n) = br (lf, n, lf)
  | avl (br (left, value, right), n) =
      let
        fun check_rotate lf = lf
          | check_rotate (br (l, v, r)) =
          let
            val diff = (depth l) - (depth r)
            fun nodeValue lf = 0 | nodeValue (br (_, v, _)) = v
          in
            if diff > 1 andalso n < nodeValue l
            then rotate (br (l, v, r), R)
            else if diff > 1 andalso n >= nodeValue l
            then rotate ((br (rotate(l, L), v, r)), R)
            else if diff < ~1 andalso n > nodeValue r
            then rotate (br (l, v, r), L)
            else if diff < ~1 andalso n <= (nodeValue r)
            then rotate ((br (l, v, rotate(r, R))), L)
            else br (l, v, r)
          end
        in
          if value > n
          then check_rotate (br (avl (left, n), value, right))
          else check_rotate (br (left, value, avl (right, n)))
        end

