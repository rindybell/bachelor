open Environment;;
open Parser;;

let normal_variables =
  (new env_tree);;

normal_variables#add "komai" (INT 22);;
