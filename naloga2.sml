(* Definition of datatypes *)
datatype natural = NEXT of natural
                 | ZERO

datatype bstree = br of bstree * int * bstree
                | lf

(* Convert natural number to integer *)
fun toInt ZERO = 0
  | toInt (NEXT n) = 1 + toInt n

(* Add two natrual numbers together *)
fun add (a, ZERO) = a
  | add (a, NEXT b) = NEXT (add (a, b))

(* Find the min in the BST *)
fun min lf = NONE
  | min (br (lf, value, _)) = SOME value
  | min (br (left, _, _)) = min left

(* Find the max in the BST *)
fun max lf = NONE
  | max (br (_, value, lf)) = SOME value
  | max (br (_, _, right)) = max right

(* Check if the BST contains a value *)
fun contains (lf, _) = false
  | contains ((br (left, value, right)), x) =
      contains (left, x) orelse value = x orelse contains (right, x)

(* Count the number of `lf` in the BST *)
fun countLeaves lf = 1
  | countLeaves (br (left, _, right)) = countLeaves left + countLeaves right

(* Count the number of `br` in BST *)
fun countBranches lf = 0
  | countBranches (br (left, _, right)) =
      countBranches left + 1 + countBranches right

(* Convert the BST to an ascending ordered list *)
fun toList lf = []
  | toList (br (left, value, right)) = (toList left)@[value]@(toList right)

(* No real need for an explanation *)
fun dropWhile _ [] = []
  | dropWhile f (x::xs) = if f x then dropWhile f xs else x::xs

(* Check if a BST is valid *)
fun valid lf = true
  | valid (br (left, value, right)) =
      let
        val lst = toList (br (left, value, right))
      in
        null (dropWhile (fn (x, y) => x < y) (ListPair.zip (lst, tl lst)))
      end

(* Create a person record for sanity *)
type person = { age : int, name : string }

(* Find the oldest individual in a list of person records *)
fun oldest [] = NONE
  | oldest lst =
      let
        fun older ((x : person), (y : person)) = if #age x > #age y then x else y
      in
        SOME (#name (foldl older (hd lst) (tl lst)))
      end

