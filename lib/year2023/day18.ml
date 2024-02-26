type dir = Up | Down | Left | Right
type input = string list

module Coord = struct
  include Utils.Coord

  let move dir meters (x, y) =
    match dir with
    | Up -> (x - meters, y)
    | Down -> (x + meters, y)
    | Left -> (x, y - meters)
    | Right -> (x, y + meters)
end

let parse filename = Utils.read_file_as_lines filename

let parse_part1 lines =
  let dir_of_char = function
    | 'U' -> Up
    | 'D' -> Down
    | 'L' -> Left
    | _ -> Right
  in
  let parse_line dir meters = (dir_of_char dir, meters) in
  List.map (fun s -> Scanf.sscanf s "%c %d" parse_line) lines

let rec dig pos = function
  | [] -> []
  | (dir, steps) :: t ->
      let pos = Coord.move dir steps pos in
      pos :: dig pos t

(* Reuse this function from day 9 *)
let rec windows = function
  | [ a; b ] -> [ (a, b) ]
  | a :: (b :: _ as l) -> (a, b) :: windows l
  | _ -> failwith "Should not happen!"

let get_boundary_points vertices =
  let dist ((x, y), (x', y')) = abs (x - x') + abs (y - y') in
  let rec aux = function [] -> 0 | hd :: tl -> dist hd + aux tl in
  aux (windows vertices)

(* Shoelace formula from day 10 *)
let get_area vertices =
  let det ((x, y), (x', y')) = (x * y') - (y * x') in
  let area2 = List.map det (windows vertices) |> List.fold_left ( + ) 0 in
  abs area2 / 2

let part_1 input =
  let lines = parse_part1 input in
  let plan = dig (0, 0) lines in
  let vertices = (0, 0) :: plan in
  let boundary_points = get_boundary_points vertices in
  let area = get_area vertices in
  Answer.of_int (area + (boundary_points / 2) + 1)

let parse_part2 lines =
  let dir_of_char = function
    | '0' -> Right
    | '1' -> Down
    | '2' -> Left
    | _ -> Up
  in
  let parse_line _ _ hex =
    let dir = dir_of_char hex.[5] in
    let meters = "0x" ^ String.sub hex 0 5 |> int_of_string in
    (dir, meters)
  in
  List.map (fun s -> Scanf.sscanf s "%c %d (#%s@)" parse_line) lines

let part_2 input =
  let lines = parse_part2 input in
  let plan = dig (0, 0) lines in
  let vertices = (0, 0) :: plan in
  let boundary_points = get_boundary_points vertices in
  let area = get_area vertices in
  Answer.of_int (area + (boundary_points / 2) + 1)
