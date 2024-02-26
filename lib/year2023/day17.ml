type input = int array array
type dir = Up | Down | Left | Right | Start

module Coord = Utils.Coord

module Node = struct
  type t = dir * Coord.t

  let compare (d, c) (d', c') =
    match Coord.compare c c' with 0 -> compare d d' | n -> n
end

module Nodes = Set.Make (Node)

module WeightedNodes = struct
  type t = Node.t * int

  let compare (n, w) (n', w') =
    match compare w w' with 0 -> Node.compare n n' | x -> x
end

module Prio = Set.Make (WeightedNodes)

let to_int c = int_of_char c - int_of_char '0'

let parse filename =
  Utils.read_file_as_lines filename
  |> Utils.to_array
  |> Array.map (Array.map to_int)

let get_dist node distances =
  match Hashtbl.find_opt distances node with Some d -> d | None -> Int.max_int

let set_dist node dist distances = Hashtbl.add distances node dist

let weight dir ax src dst map =
  let ws =
    match dir with
    | Up -> List.init (src - dst) (fun i -> map.(src - i - 1).(ax))
    | Down -> List.init (dst - src) (fun i -> map.(src + i + 1).(ax))
    | Left -> List.init (src - dst) (fun i -> map.(ax).(src - i - 1))
    | Right -> List.init (dst - src) (fun i -> map.(ax).(src + i + 1))
    | Start -> failwith "weight"
  in
  List.fold_left ( + ) 0 ws

let u (i, j) map =
  List.init 3 (fun s ->
      if i - s <= 0 then None
      else Some ((Up, (i - s - 1, j)), weight Up j i (i - s - 1) map))

let d (i, j) map h =
  List.init 3 (fun s ->
      if i + s + 1 >= h then None
      else Some ((Down, (i + s + 1, j)), weight Down j i (i + s + 1) map))

let l (i, j) map =
  List.init 3 (fun s ->
      if j - s <= 0 then None
      else Some ((Left, (i, j - s - 1)), weight Left i j (j - s - 1) map))

let r (i, j) map w =
  List.init 3 (fun s ->
      if j + s + 1 >= w then None
      else Some ((Right, (i, j + s + 1)), weight Right i j (j + s + 1) map))

let neighbors (dir, pos) map (w, h) =
  List.filter_map Fun.id
    (match dir with
    | Up | Down -> l pos map @ r pos map w
    | Left | Right -> u pos map @ d pos map h
    | Start -> u pos map @ d pos map h @ l pos map @ r pos map w)

let rec dijkstra map w_h distances visited todo =
  let cur, d = Prio.min_elt todo in
  let visited = Nodes.add cur visited in
  let neighborhood =
    List.filter
      (fun (n, _) -> not (Nodes.mem n visited))
      (neighbors cur map w_h)
  in
  let todo =
    List.fold_left
      (fun todo (n', w') ->
        if d + w' < get_dist n' distances then (
          set_dist n' (d + w') distances;
          Prio.add (n', d + w') todo)
        else todo)
      (Prio.remove (cur, d) todo)
      neighborhood
  in
  if Prio.is_empty todo then () else dijkstra map w_h distances visited todo

let part_1 input =
  let width, height = (Array.length input.(0), Array.length input) in
  let src = (0, 0) in
  let dst = (height - 1, width - 1) in
  let distances = Hashtbl.create 4096 in
  let visited = Nodes.empty in
  let prio = Prio.singleton ((Start, src), 0) in
  set_dist (Start, src) 0 distances;
  dijkstra input (width, height) distances visited prio;
  min (get_dist (Down, dst) distances) (get_dist (Right, dst) distances)
  |> Answer.of_int

let part_2 _ = Answer.of_int 0
