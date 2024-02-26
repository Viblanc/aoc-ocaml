module Ints = Set.Make (Int)

type input = (Ints.t * Ints.t) list

let rec get_score = function 0 -> 0 | 1 -> 1 | n -> 2 * get_score (n - 1)

let parse_cards lines =
  let parse_card line =
    let to_ints s = Utils.find_numbers s |> Ints.of_list in
    let parse_winning winning =
      match Str.split (Str.regexp ": ") winning with
      | [ _; w ] -> to_ints w
      | _ -> failwith "Should not happen"
    in
    match Str.split (Str.regexp " | ") line with
    | [ w; n ] -> (parse_winning w, to_ints n)
    | _ -> failwith "Should not happen either"
  in
  List.map parse_card lines

let parse filename = Utils.read_file_as_lines filename |> parse_cards

let part_1 input =
  List.map (fun (w, n) -> Ints.inter w n |> Ints.cardinal |> get_score) input
  |> List.fold_left ( + ) 0 |> Answer.of_int

let rec add_copies (i, n) cards =
  match (i, cards) with
  | _, [] -> []
  | 0, _ -> cards
  | i, (i', n') :: cs -> (i', n + n') :: add_copies (i - 1, n) cs

let sum_cards cards =
  let rec aux acc = function
    | [] -> acc
    | ((_, n) as c) :: cs -> aux (acc + n) (add_copies c cs)
  in
  aux 0 cards

let part_2 input =
  List.map (fun (w, n) -> (Ints.inter w n |> Ints.cardinal, 1)) input
  |> sum_cards |> Answer.of_int
