type input = char array array

module Seen = Set.Make (Utils.Coord)

let rec to_int = function
  | 'S' -> to_int 'a'
  | 'E' -> to_int 'z'
  | c -> int_of_char c - int_of_char 'a'

let parse filename =
  Utils.read_file_as_lines filename
  |> List.map (fun s -> String.to_seq s |> Array.of_seq)
  |> Array.of_list

let find_tile grid tile =
  Array.find_mapi
    (fun i arr ->
      Array.find_mapi (fun j c -> if c = tile then Some (i, j) else None) arr)
    grid
  |> Option.value ~default:(0, 0)

let rec bfs vertices grid seen =
  let height, width = (Array.length grid, Array.length grid.(0)) in
  let in_bounds (x, y) = x >= 0 && y >= 0 && x < height && y < width in
  let neighbors (x, y) =
    List.filter
      (fun pos ->
        (not (Seen.mem pos seen))
        && in_bounds pos
        && to_int grid.(fst pos).(snd pos) - to_int grid.(x).(y) < 2)
      [ (x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1) ]
    |> List.to_seq
  in
  match Queue.take vertices with
  | exception Queue.Empty -> failwith "not found"
  | dist, pos ->
      let x, y = pos in
      if grid.(x).(y) = 'E' then dist
      else
        let neighbors = neighbors pos in
        let seen = Seen.add_seq neighbors seen in
        Queue.add_seq vertices
          (Seq.map (fun neighbor -> (dist + 1, neighbor)) neighbors);
        bfs vertices grid seen

let part_1 input =
  let start = find_tile input 'S' in
  let vertices = Queue.create () in
  Queue.add (0, start) vertices;
  Answer.Int (bfs vertices input Seen.empty)

let rec bfs vertices grid seen =
  let height, width = (Array.length grid, Array.length grid.(0)) in
  let in_bounds (x, y) = x >= 0 && y >= 0 && x < height && y < width in
  let neighbors (x, y) =
    List.filter
      (fun pos ->
        (not (Seen.mem pos seen))
        && in_bounds pos
        && to_int grid.(x).(y) - to_int grid.(fst pos).(snd pos) < 2)
      [ (x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1) ]
    |> List.to_seq
  in
  match Queue.take vertices with
  | exception Queue.Empty -> failwith "not found"
  | dist, pos ->
      let x, y = pos in
      if grid.(x).(y) = 'a' || grid.(x).(y) = 'S' then dist
      else
        let neighbors = neighbors pos in
        let seen = Seen.add_seq neighbors seen in
        Queue.add_seq vertices
          (Seq.map (fun neighbor -> (dist + 1, neighbor)) neighbors);
        bfs vertices grid seen

let part_2 input =
  let start = find_tile input 'E' in
  let vertices = Queue.create () in
  Queue.add (0, start) vertices;
  Answer.Int (bfs vertices input Seen.empty)
