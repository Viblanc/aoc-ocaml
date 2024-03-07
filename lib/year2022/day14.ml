module Coord = struct
  type t = int * int

  let compare (x, y) (x', y') =
    match compare y y' with 0 -> compare x x' | n -> n
end

module S = Set.Make (Coord)

type input = S.t

let parse filename =
  let range_to_list ((x, y), (x', y')) =
    match compare x x' with
    | 0 -> List.init (abs (y - y') + 1) (fun i -> (x, min y y' + i))
    | _ -> List.init (abs (x - x') + 1) (fun i -> (min x x' + i, y))
  in
  let rec windows = function
    | a :: (b :: _ as l) -> (a, b) :: windows l
    | _ -> []
  in
  let scan n m = (n, m) in
  Utils.read_file_as_lines filename
  |> List.map (fun line ->
         Str.split (Str.regexp " -> ") line
         |> List.map (fun s -> Scanf.sscanf s "%d,%d" scan))
  |> List.map windows |> List.flatten |> List.map range_to_list |> List.flatten
  |> S.of_list

let rec produce_sand pos map in_abyss at_bottom =
  if in_abyss pos || S.mem (500, 0) map then (map, true)
  else
    let x, y = pos in
    let tiles = [ (x, y + 1); (x - 1, y + 1); (x + 1, y + 1) ] in
    match List.find_opt (fun tile -> not (S.mem tile map)) tiles with
    | None -> (S.add pos map, false)
    | Some pos ->
        if at_bottom pos then (S.add pos map, false)
        else produce_sand pos map in_abyss at_bottom

let rec simulate_sand map in_abyss at_bottom =
  let map, over = produce_sand (500, 0) map in_abyss at_bottom in
  if over then map else simulate_sand map in_abyss at_bottom

let part_1 input =
  let rocks = S.cardinal input in
  let _, lowest = S.max_elt input in
  let in_abyss (_, y) = y >= lowest in
  let at_bottom _ = false in
  let map = simulate_sand input in_abyss at_bottom in
  Answer.Int (S.cardinal map - rocks)

let part_2 input =
  let rocks = S.cardinal input in
  let _, lowest = S.max_elt input in
  let lowest = lowest + 2 in
  let in_abyss _ = false in
  let at_bottom (_, y) = y = lowest - 1 in
  let map = simulate_sand input in_abyss at_bottom in
  Answer.Int (S.cardinal map - rocks)
