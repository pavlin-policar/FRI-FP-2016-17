(* Definition of datatypes *)
datatype natural = NEXT of natural
                 | ZERO

datatype bstree = br of bstree * int * bstree
                | lf

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

(* Check if a BST is valid *)
fun valid lf = true
  | valid (br (left, value, right)) =
      let
        fun valueOf (br (_, v, _)) = v | valueOf lf = ~1
        fun ordered (lf, _, lf) = true
          | ordered (left, value, lf) = valueOf left < value
          | ordered (lf, value, right) = value < valueOf right
          | ordered (left, value, right) =
              valueOf left < value andalso value < valueOf right
      in
        valid left andalso valid right andalso ordered (left, value, right)
      end


(* Create a person record for sanity *)
type person = { age : int, name : string }

(* Find the oldest individual in a list of person records *)
fun oldest [] = NONE
  | oldest (lst : person list) =
      let
        fun older ((x : person), (y : person)) = if #age x > #age y then x else y
      in
        SOME (#name (foldl older (hd lst) (tl lst)))
      end
