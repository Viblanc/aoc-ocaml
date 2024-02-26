type dir = N | E | S | W
type pipe = NS | WE | NE | NW | SW | SE | Start

module Coord = struct
  type t = int * int

  let move (x, y) = function
    | N -> (x - 1, y)
    | S -> (x + 1, y)
    | E -> (x, y + 1)
    | W -> (x, y - 1)

  let compare (x, y) (x', y') =
    match Int.compare x x' with 0 -> Int.compare y y' | n -> n
end

module Tiles = Map.Make (Coord)

type input = pipe Tiles.t

let get_type (k, v) =
  match v with
  | '|' -> Some (k, NS)
  | '-' -> Some (k, WE)
  | 'L' -> Some (k, NE)
  | 'J' -> Some (k, NW)
  | '7' -> Some (k, SW)
  | 'F' -> Some (k, SE)
  | 'S' -> Some (k, Start)
  | _ -> None

let parse filename =
  Utils.read_file_as_lines filename
  |> List.map (fun s -> String.to_seq s |> List.of_seq)
  |> List.mapi (fun i l -> List.mapi (fun j c -> ((i, j), c)) l)
  |> List.flatten |> List.filter_map get_type |> Tiles.of_list

let goto_next pipe dir =
  match (pipe, dir) with
  | NS, N | NE, W | NW, E -> Some N
  | NS, S | SW, E | SE, W -> Some S
  | WE, E | NE, S | SE, N -> Some E
  | WE, W | NW, S | SW, N -> Some W
  | Start, _ -> Some dir
  | _ -> None

let starting_tile tiles =
  let start = Tiles.to_list tiles |> List.find (fun (_, c) -> c = Start) in
  let is_possible dir =
    let pos = Coord.move (fst start) dir in
    match Tiles.find_opt pos tiles with
    | None -> false
    | Some pipe -> Option.is_some (goto_next pipe dir)
  in
  (start, List.find is_possible [ N; S; E; W ])

let find_loop tiles =
  let start_tile, start_dir = starting_tile tiles in
  let rec aux ((pos, _) as tile) dir seen =
    let nxt_pos = Coord.move pos dir in
    match Tiles.find_opt nxt_pos tiles with
    | Some Start -> tile :: seen
    | Some nxt_pipe ->
        let nxt_dir = Option.get @@ goto_next nxt_pipe dir in
        aux (nxt_pos, nxt_pipe) nxt_dir (tile :: seen)
    | None -> failwith "Broke out of the loop"
  in
  aux start_tile start_dir []

let loop_len loop = List.length loop / 2
let part_1 input = find_loop input |> loop_len |> Answer.of_int

(* Reuse this function from day 9 *)
let rec windows = function
  | [ a; b ] -> [ (a, b) ]
  | a :: (b :: _ as l) -> (a, b) :: windows l
  | _ -> failwith "Should not happen!"

(* Shoelace formula *)
let get_area tiles =
  let det ((x, y), (x', y')) = (x * y') - (y * x') in
  let start_tile, _ = starting_tile tiles in
  let vertices =
    start_tile :: find_loop tiles
    |> List.filter (fun (_, pipe) -> pipe <> NS && pipe <> WE)
    |> List.map fst |> windows
  in
  List.fold_left (fun acc pair -> acc + det pair) 0 vertices / 2

(* Pick's theorem: A = i + b/2 - 1 ==> i = A - b/2 + 1 *)
let part_2 input =
  let loop = find_loop input in
  let area = get_area input in
  area - loop_len loop + 1 |> Answer.of_int
