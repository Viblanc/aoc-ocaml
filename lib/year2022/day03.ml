type input = string list

module S = Set.Make (Char)

let parse filename = Utils.read_file_as_lines filename

let split s =
  let half = String.length s / 2 in
  (String.sub s 0 half, String.sub s half half)

let find_dup s =
  let a, b = split s in
  S.inter (S.of_seq (String.to_seq a)) (S.of_seq (String.to_seq b)) |> S.choose
(*   String.to_seq a |> List.of_seq |> List.find (String.contains b) *)

let get_priority c =
  let n = int_of_char c in
  match c with
  | 'a' .. 'z' -> n - int_of_char 'a' + 1
  | _ -> n - int_of_char 'A' + 27

let part_1 input =
  List.map find_dup input |> List.map get_priority |> List.fold_left ( + ) 0
  |> Answer.of_int

let group_by_3 l =
  let rec aux acc = function
    | [] -> acc
    | a :: b :: c :: t -> aux ((a, b, c) :: acc) t
    | _ -> failwith "should not happen"
  in
  aux [] l

let find_dup (a, b, c) =
  S.inter (S.of_seq (String.to_seq a)) (S.of_seq (String.to_seq b))
  |> S.inter (S.of_seq (String.to_seq c))
  |> S.choose

let part_2 input =
  let input = group_by_3 input in
  List.map find_dup input |> List.map get_priority |> List.fold_left ( + ) 0
  |> Answer.of_int
