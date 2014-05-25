(* For ""******"" -> "******" *)
let twin_dblq_to_single_dblq a_string =
  let a_string_length = String.length a_string in
    String.sub a_string 1 (a_string_length - 2)
;;
