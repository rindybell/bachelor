(* open external module *)
open Environment;;

type 'a statement =
    Atom of 'a
  | Cons of 'a statement * 'a statement
  | Nil
;;

type 'a contiflow =
    Pair of 'a statement * 'a Environment.env_tree
  | Pairs of 'a contiflow * 'a contiflow
  | AppPair of 'a * 'a contiflow
  | PairArg of 'a contiflow * 'a statement
  | None
;;
