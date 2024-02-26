type input = string list

let int_of_card = function
  | 'A' -> 14
  | 'K' -> 13
  | 'Q' -> 12
  | 'J' -> 11
  | 'T' -> 10
  | c -> int_of_char c - int_of_char '0'

let get_type cards =
  let rec aux = function
    | [] -> []
    | h :: t ->
        let n = 1 + (List.filter (fun i -> i = h) t |> List.length) in
        n :: aux (List.filter (fun i -> i <> h) t)
  in
  match List.sort (fun a b -> compare b a) (aux cards) with
  | 5 :: _ -> 6
  | 4 :: _ -> 5
  | 3 :: 2 :: _ -> 4
  | 3 :: _ -> 3
  | 2 :: 2 :: _ -> 2
  | 2 :: _ -> 1
  | _ -> 0

let cards_of_string s =
  let parse cards bid =
    (String.to_seq cards |> List.of_seq |> List.map int_of_card, bid)
  in
  Scanf.sscanf s "%s %d" parse

let compare_cards (c, _) (c', _) =
  let rec compare_lists = function
    | [], _ | _, [] -> 0
    | h :: t, h' :: t' when h = h' -> compare_lists (t, t')
    | h :: _, h' :: _ -> Int.compare h h'
  in
  let t, t' = (get_type c, get_type c') in
  match Int.compare t t' with 0 -> compare_lists (c, c') | n -> n

let parse filename = Utils.read_file_as_lines filename

let part_1 input =
  List.map cards_of_string input
  |> List.sort (fun a b -> compare_cards a b)
  |> List.mapi (fun i (_, b) -> (i + 1) * b)
  |> List.fold_left ( + ) 0 |> Answer.of_int

let int_of_card = function
  | 'A' -> 14
  | 'K' -> 13
  | 'Q' -> 12
  | 'J' -> 1
  | 'T' -> 10
  | c -> int_of_char c - int_of_char '0'

let cards_of_string s =
  let parse cards bid =
    (String.to_seq cards |> List.of_seq |> List.map int_of_card, bid)
  in
  Scanf.sscanf s "%s %d" parse

let get_type cards =
  let rec aux = function
    | [] -> []
    | h :: t ->
        let n = 1 + (List.filter (fun i -> i = h) t |> List.length) in
        (n, h) :: aux (List.filter (fun i -> i <> h) t)
  in
  let cmp (x, y) (x', y') =
    match compare x' x with 0 -> compare y y' | n -> n
  in
  match List.sort cmp (aux cards) with
  | (5, _) :: _ -> 6
  | (4, c) :: (1, c') :: _ when c = 1 || c' = 1 -> 6
  | (4, _) :: _ -> 5
  | (3, c) :: (2, c') :: _ when c = 1 || c' = 1 -> 6
  | (3, _) :: (2, _) :: _ -> 4
  | (3, c) :: (1, c') :: _ when c = 1 || c' = 1 -> 5
  | (3, _) :: _ -> 3
  | (2, c) :: (2, c') :: _ when c = 1 || c' = 1 -> 5
  | (2, _) :: (2, _) :: (1, 1) :: _ -> 4
  | (2, _) :: (2, _) :: _ -> 2
  | (2, c) :: (1, c') :: _ when c = 1 || c' = 1 -> 3
  | (2, _) :: _ -> 1
  | (1, 1) :: _ -> 1
  | _ -> 0

let compare_cards (c, _) (c', _) =
  let rec compare_lists = function
    | [], _ | _, [] -> 0
    | h :: t, h' :: t' when h = h' -> compare_lists (t, t')
    | h :: _, h' :: _ -> Int.compare h h'
  in
  let t, t' = (get_type c, get_type c') in
  match Int.compare t t' with 0 -> compare_lists (c, c') | n -> n

let part_2 input =
  List.map cards_of_string input
  |> List.sort (fun a b -> compare_cards a b)
  |> List.mapi (fun i (_, b) -> (i + 1) * b)
  |> List.fold_left ( + ) 0 |> Answer.of_int
