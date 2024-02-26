type tile = S | G | R

module Grid = Utils.Grid
module Steps = Set.Make (Utils.Coord)

type input = tile Grid.t

let parse filename =
  let char_of_tile = function 'S' -> S | '.' -> G | _ -> R in
  Utils.read_file_as_lines filename
  |> List.map (fun s -> String.to_seq s |> List.of_seq |> List.map char_of_tile)
  |> Grid.of_nested_list

let take_step n grid =
  let start, _ = Grid.to_list grid |> List.find (fun (_, t) -> t = S) in
  let add_steps pos steps =
    let positions = Grid.get_neighbors pos grid in
    List.fold_left
      (fun acc pos ->
        match Grid.find_opt pos grid with
        | Some R -> acc
        | _ -> Steps.add pos acc)
      steps positions
  in
  let rec aux n steps positions =
    match (n, positions) with
    | 0, _ -> List.length positions
    | 1, [] -> Steps.cardinal steps
    | _, [] -> aux (n - 1) Steps.empty (Steps.to_list steps)
    | n, p :: rest -> aux n (add_steps p steps) rest
  in
  aux n Steps.empty [ start ]

let part_1 input = Answer.of_int (take_step 64 input)
let part_2 _input = Answer.of_int 0
(*let size = Grid.width input + Grid.height input in
  let n = 26501365 / size in
  let rest = abs (26501365 mod size) in
  let y0 = take_step ((0 * size) + rest) input in
  let y1 = take_step ((1 * size) + rest) input in
  let y2 = take_step ((2 * size) + rest) input in
  let a = (y0 - (2 * y1) + y2) / 2 in
  let b = ((4 * y1) - (3 * y0) - y2) / 2 in
  let c = y0 in*)
(*(a * (n * n)) + (b * n) + c*)
