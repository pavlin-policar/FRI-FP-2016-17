(* Calling List.* is a drag *)
val filter = List.filter

(* Prefix functions allow partial application *)
fun lt x y = x < y
fun gte x y = x >= y

(* NALOGA 4 *)

(* Keep only even numbers in a list *)
val only_even = filter (fn x => x mod 2 = 0)

fun longest_string_helper f =
  foldl (fn (x, a) => if f (size x, size a) then x else a) ""

val longest_string_first = longest_string_helper (fn (x, y) => x > y)
val longest_string_last = longest_string_helper (fn (x, y) => x >= y)

fun quicksort [] = []
  | quicksort (x::xs) =
    (quicksort (filter (gte x) xs))@[x]@(quicksort (filter (lt x) xs))

(* Dot product of two vectors *)
fun dot x y = foldl op+ 0 (ListPair.map op* (x, y))

(* Transpose a matrix *)
fun transpose ([]::_) = []
  | transpose rows = (map hd rows)::(transpose (map tl rows))

(* Multiply two matrices *)
fun multiply x y = map (fn xi => map (fn yj => dot xi yj) (transpose y)) x

(* TODO This is hideous *)
(* Group consecutive elements in a list *)
fun group [] = []
  | group (x::xs) =
      rev (foldl (fn (x, y as ((y0, n)::ys)) => if x = y0
                      then (x, n + 1)::ys
                      else (x, 1)::y) [(x, 1)] xs)

(* TODO The type doesn't match - 'a vs ''a *)
(* Generate equivalence lists for a list given an equivalence function *)
fun equivalence_classes f xs = map (fn x => filter (fn y => f x y) xs) xs

(* test *)
fun equiv x y = x mod 3 = y mod 3
fun equiv x y = x = y mod 3

val test = equivalence_classes equiv [1,2,3,4,5,6,7,8,9,10,11,12,13,14]
