type input = (int * int) list

module Coord = struct
  include Utils.Coord

  let ( + ) (x, y) (x', y') = (x + x', y + y')
end

module S = Set.Make (Coord)

let move_rope_in_dir (head, tail) dir =
  let sign n = Int.compare n 0 in
  let head = Coord.(head + dir) in
  let rec aux (hx, hy) = function
    | [] -> []
    | (x, y) :: l ->
        let dx, dy = (hx - x, hy - y) in
        if abs dx < 2 && abs dy < 2 then (x, y) :: aux (x, y) l
        else
          let x, y = (x + sign dx, y + sign dy) in
          (x, y) :: aux (x, y) l
  in
  (head, aux head tail)

let parse filename =
  let scan dir n =
    match dir with
    | 'U' -> List.init n (fun _ -> (-1, 0))
    | 'D' -> List.init n (fun _ -> (1, 0))
    | 'L' -> List.init n (fun _ -> (0, -1))
    | _ -> List.init n (fun _ -> (0, 1))
  in
  Utils.read_file_as_lines filename
  |> List.map (fun s -> Scanf.sscanf s "%c %d" scan)
  |> List.flatten

let rec move_rope rope = function
  | [] -> []
  | dir :: dirs ->
      let rope = move_rope_in_dir rope dir in
      List.(hd (rev (snd rope))) :: move_rope rope dirs

let part_1 input =
  move_rope ((0, 0), [ (0, 0) ]) input
  |> S.of_list |> S.cardinal |> Answer.of_int

let part_2 input =
  move_rope ((0, 0), List.init 9 (fun _ -> (0, 0))) input
  |> S.of_list |> S.cardinal |> Answer.of_int
