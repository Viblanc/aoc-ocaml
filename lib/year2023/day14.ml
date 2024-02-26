type tile = Round | Cube | Empty
type input = tile list list

let parse = function 'O' -> Round | '#' -> Cube | _ -> Empty

let parse filename =
  Utils.read_file_as_lines filename
  |> List.map (fun s -> String.to_seq s |> List.of_seq |> List.map parse)

let transpose l =
  let len = List.length (List.hd l) in
  List.init len (fun i -> List.map (Fun.flip List.nth i) l |> List.rev)

let tilt_north plat =
  let move_round_rocks a b =
    match (a, b) with Round, Empty -> 1 | Empty, Round -> -1 | _ -> 0
  in
  let tilt_row row =
    let rec aux acc = function
      | [] -> List.sort move_round_rocks acc
      | Cube :: tl -> List.sort move_round_rocks acc @ (Cube :: aux [] tl)
      | hd :: tl -> aux (hd :: acc) tl
    in
    aux [] row
  in
  transpose plat |> List.map tilt_row

let get_load plat =
  List.fold_left
    (fun acc arr ->
      acc
      + (List.mapi (fun i c -> if c = Round then i + 1 else 0) arr
        |> List.fold_left ( + ) 0))
    0 plat

let part_1 input = get_load (tilt_north input) |> Answer.of_int
let do_cycle plat = tilt_north plat |> tilt_north |> tilt_north |> tilt_north

let run_spin_cycle n plat =
  let mem = Hashtbl.create 256 in
  let rec get_loop_len plat n =
    match Hashtbl.find_opt mem plat with
    | Some s -> (s, n)
    | None ->
        Hashtbl.add mem plat n;
        get_loop_len (do_cycle plat) (n + 1)
  in
  let start, loop = get_loop_len plat 0 in
  let cycle = start + ((n - loop) mod (loop - start)) in
  Hashtbl.fold (fun p n a -> if n = cycle then p else a) mem []
  |> transpose |> get_load

let part_2 input = run_spin_cycle 1_000_000_000 input |> Answer.of_int
