type dir = Up | Down | Left | Right
type tile = Empty | Mir | RMir | Hor | Ver

module Coord = struct
  include Utils.Coord

  let move (x, y) = function
    | Up -> (x - 1, y)
    | Down -> (x + 1, y)
    | Left -> (x, y - 1)
    | Right -> (x, y + 1)
end

module Coords = Set.Make (Coord)
module Grid = Utils.Grid

module Move = struct
  type t = dir * Coord.t

  let compare (d, c) (d', c') =
    match compare d d' with 0 -> compare c c' | n -> n
end

module Seen = Set.Make (Move)

type input = tile Grid.t

let to_tile = function
  | '.' -> Empty
  | '/' -> Mir
  | '\\' -> RMir
  | '-' -> Hor
  | _ -> Ver

let parse filename =
  Utils.read_file_as_lines filename
  |> List.map (fun s -> String.to_seq s |> List.of_seq |> List.map to_tile)
  |> Grid.of_nested_list

let rec step pos dir seen grid =
  let pos = Coord.move pos dir in
  if Seen.mem (dir, pos) seen then seen
  else
    match Grid.find_opt pos grid with
    | None -> seen
    | Some t -> (
        let seen = Seen.add (dir, pos) seen in
        match t with
        | Empty -> step pos dir seen grid
        | Mir -> (
            match dir with
            | Up -> step pos Right seen grid
            | Down -> step pos Left seen grid
            | Left -> step pos Down seen grid
            | Right -> step pos Up seen grid)
        | RMir -> (
            match dir with
            | Up -> step pos Left seen grid
            | Down -> step pos Right seen grid
            | Left -> step pos Up seen grid
            | Right -> step pos Down seen grid)
        | Hor -> (
            match dir with
            | Up | Down -> step pos Left (step pos Right seen grid) grid
            | _ -> step pos dir seen grid)
        | Ver -> (
            match dir with
            | Left | Right -> step pos Up (step pos Down seen grid) grid
            | _ -> step pos dir seen grid))

let part_1 input =
  let energized_tiles = step (0, -1) Right Seen.empty input in
  Seen.to_list energized_tiles
  |> List.map snd |> Coords.of_list |> Coords.cardinal |> Answer.of_int

let part_2 input =
  let width, height = (Grid.width input, Grid.height input) in
  let from_top = Seq.ints 0 |> Seq.zip (Seq.repeat (-1)) |> Seq.take width in
  let from_bot = Seq.ints 0 |> Seq.zip (Seq.repeat height) |> Seq.take width in
  let from_left = Seq.repeat (-1) |> Seq.zip (Seq.ints 0) |> Seq.take height in
  let from_right =
    Seq.repeat width |> Seq.zip (Seq.ints 0) |> Seq.take height
  in
  let get_max_energized_tiles seq dir =
    let energized s =
      Seen.to_list s |> List.map snd |> Coords.of_list |> Coords.cardinal
    in
    Seq.fold_left
      (fun acc pos -> max acc (energized (step pos dir Seen.empty input)))
      0 seq
  in
  Int.max
    (Int.max
       (get_max_energized_tiles from_top Down)
       (get_max_energized_tiles from_bot Up))
    (Int.max
       (get_max_energized_tiles from_left Right)
       (get_max_energized_tiles from_right Left))
  |> Answer.of_int
