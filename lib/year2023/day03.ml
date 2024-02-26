type input = string list

module Coord = struct
  type t = int * int

  let compare (x, y) (x', y') =
    match Int.compare x x' with 0 -> Int.compare y y' | n -> n
end

module Coords = Set.Make (Coord)

type symbol = Gear of Coords.t | Num of (int * Coords.t)

let get_neighbors (x, y) =
  Coords.of_list
    [
      (x - 1, y - 1);
      (x - 1, y);
      (x - 1, y + 1);
      (x, y + 1);
      (x + 1, y + 1);
      (x + 1, y);
      (x + 1, y - 1);
      (x, y - 1);
    ]

let is_symbol = function '0' .. '9' | '.' -> false | _ -> true

let find_symbols lines is_symbol =
  List.mapi
    (fun i line ->
      String.to_seq line |> List.of_seq
      |> List.mapi (fun j c -> (i, j, is_symbol c)))
    lines
  |> List.flatten
  |> List.filter_map (fun (i, j, is_sym) ->
         if is_sym then Some (Gear (get_neighbors (i, j))) else None)

let find_numbers lines =
  let rec find_numbers_in_line line start =
    match Str.search_forward (Str.regexp {|[0-9]+|}) line start with
    | m_beg ->
        let m_end = Str.match_end () in
        let n = int_of_string @@ Str.matched_string line in
        (n, List.init (m_end - m_beg) (fun i -> i + m_beg))
        :: find_numbers_in_line line m_end
    | exception Not_found -> []
  in
  List.mapi
    (fun i l ->
      let nums = find_numbers_in_line l 0 in
      List.map
        (fun (n, idxs) ->
          let coords = Coords.of_list (List.map (fun j -> (i, j)) idxs) in
          Num (n, coords))
        nums)
    lines
  |> List.flatten

let is_part_number = function
  | Num (n, cs), Gear cs' -> if Coords.disjoint cs cs' then None else Some n
  | _ -> failwith "Something unexpected occurred"

let parse filename = Utils.read_file_as_lines filename

let part_1 input =
  let symbols = find_symbols input is_symbol in
  let numbers = find_numbers input in
  List.map
    (fun num ->
      List.fold_left
        (fun acc sym ->
          acc + Option.value ~default:0 (is_part_number (num, sym)))
        0 symbols)
    numbers
  |> List.fold_left ( + ) 0 |> Answer.of_int

let is_symbol c = c = '*'

let part_2 input =
  let symbols = find_symbols input is_symbol in
  let numbers = find_numbers input in
  List.filter_map
    (fun sym ->
      let nums =
        List.filter_map (fun num -> is_part_number (num, sym)) numbers
      in
      match List.length nums with
      | 2 -> Some (List.fold_left Int.mul 1 nums)
      | _ -> None)
    symbols
  |> List.fold_left ( + ) 0 |> Answer.of_int
