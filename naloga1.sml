(* Naloga 1 *)

(* n! *)
fun factorial 0 = 1
  | factorial n = n * factorial (n - 1)

(* Power *)
fun power (x, 0) = 1
  | power (x, n) = x * power (x, (n - 1))

(* GCD using Euclidean method *)
fun gcd (a, 0) = a
  | gcd (a, b) = gcd (b, (a mod b))

(* Length of a list without using length *)
fun len xs = foldr (fn(_, y) => y + 1) 0 xs

(* Last element of the list, if it exists *)
fun last ([] : int list) = NONE
  | last (x::[]) = SOME x
  | last (_::xs) = last xs

(* N-th element of list, first element index 0 *)
fun nth ([], _) = NONE
  | nth ((x::_), 0) = SOME x
  | nth ((_::xs), n) = nth (xs, (n - 1))

(* Insert nx into n-th position, 0 puts it in first place, etc... *)
fun insert (lst, 0, nx) = nx::lst
  | insert ((x::xs), n, nx) = x::(insert (xs, (n - 1), nx))
  | insert ([], _, _) = []

(* Delete element from list *)
fun delete ([] : int list, _) = []
  | delete ((x::xs), n) = if x = n then delete (xs, n) else x::(delete (xs, n))

(* Reverse a list *)
fun reverse [] = []
  | reverse (x::xs) = (reverse xs)@[x]

(* Check if list is a palindrome *)
fun palindrome (lst : int list) = lst = (reverse lst)

