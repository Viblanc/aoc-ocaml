(*module Grid = Utils.Grid
  module Coord = Utils.Coord
  module Seen = Set.Make (Coord)*)

type input = int array array

let parse filename =
  let to_int c = int_of_char c - int_of_char '0' in
  Utils.read_file_as_lines filename
  |> List.map (fun s -> String.to_seq s |> Array.of_seq |> Array.map to_int)
  |> Array.of_list

let is_visible (x, y) grid =
  let h, w = (Array.length grid, Array.length grid.(0)) in
  let height = grid.(x).(y) in
  let out_of_bounds (x, y) = x < 0 || y < 0 || x = h || y = w in
  let rec aux height (x, y) (dx, dy) =
    if out_of_bounds (x, y) then true
    else
      match compare height grid.(x).(y) with
      | 1 -> aux height (x + dx, y + dy) (dx, dy)
      | _ -> false
  in
  List.exists
    (fun (dx, dy) -> aux height (x + dx, y + dy) (dx, dy))
    [ (-1, 0); (1, 0); (0, -1); (0, 1) ]

let part_1 input =
  let h, w = (Array.length input, Array.length input.(0)) in
  List.init h (fun i -> List.init w (fun j -> (i, j)))
  |> List.flatten
  |> List.filter (fun pos -> is_visible pos input)
  |> List.length |> Answer.of_int

let scenic_score (x, y) grid =
  let h, w = (Array.length grid, Array.length grid.(0)) in
  let height = grid.(x).(y) in
  let out_of_bounds (x, y) = x < 0 || y < 0 || x = h || y = w in
  let rec aux height (x, y) (dx, dy) =
    if out_of_bounds (x, y) then 0
    else
      match compare height grid.(x).(y) with
      | 1 -> 1 + aux height (x + dx, y + dy) (dx, dy)
      | _ -> 1
  in
  List.fold_left
    (fun acc (dx, dy) -> acc * aux height (x + dx, y + dy) (dx, dy))
    1
    [ (-1, 0); (1, 0); (0, -1); (0, 1) ]

let part_2 input =
  let h, w = (Array.length input, Array.length input.(0)) in
  List.init h (fun i -> List.init w (fun j -> (i, j)))
  |> List.flatten
  |> List.fold_left (fun acc pos -> max acc (scenic_score pos input)) 0
  |> Answer.of_int
