let read_file_as_lines filename =
  In_channel.with_open_bin filename In_channel.input_lines

let read_file filename = In_channel.with_open_bin filename In_channel.input_all

let to_array l =
  List.map (fun s -> String.to_seq s |> Array.of_seq) l |> Array.of_list

let get_lines s =
  let len = String.length s in
  String.split_on_char '\n' (String.sub s 0 (len - 1))

let find_numbers s =
  let re = Str.regexp {|[0-9]+|} in
  let rec aux acc start =
    match Str.search_forward re s start with
    | _ ->
        aux ((int_of_string @@ Str.matched_string s) :: acc) (Str.match_end ())
    | exception Not_found -> List.rev acc
  in
  aux [] 0

module Coord = struct
  type t = int * int

  let compare (x, y) (x', y') =
    match Int.compare x x' with 0 -> Int.compare y y' | n -> n
end

module Grid = struct
  module G = Map.Make (Coord)

  type 'a t = { width : int; height : int; grid : 'a G.t }

  let width { width; height = _; grid = _ } = width
  let height { width = _; height; grid = _ } = height

  let of_nested_list l =
    let height, width = (List.length l, List.length (List.hd l)) in
    let grid =
      List.mapi (fun i ll -> List.mapi (fun j a -> ((i, j), a)) ll) l
      |> List.flatten |> G.of_list
    in
    { width; height; grid }

  let find_opt pos { width = _; height = _; grid } = G.find_opt pos grid
  let to_list { width = _; height = _; grid } = G.to_list grid

  let of_list l =
    let width = 1 + (List.map fst l |> List.map snd |> List.fold_left max 0) in
    let height = 1 + (List.map fst l |> List.map fst |> List.fold_left max 0) in
    let grid = G.of_list l in
    { width; height; grid }

  let get_neighbors (x, y) grid =
    let neighbors = [ (x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1) ] in
    let rec aux acc = function
      | [] -> acc
      | h :: t -> (
          match find_opt h grid with
          | None -> aux acc t
          | Some _ -> aux (h :: acc) t)
    in
    aux [] neighbors
end
