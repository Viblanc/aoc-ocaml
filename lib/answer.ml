type t = Int of int | Str of string

let of_int n = Int n
let of_string s = Str s

let pp_answer fmt = function
  | Int n -> Format.fprintf fmt "%d" n
  | Str s -> Format.fprintf fmt "%s" s

let eq a b = a = b
