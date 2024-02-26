type input = int list * (int * int * int) list list

let parse filename =
  let l = Utils.read_file filename |> Str.split (Str.regexp "\n\n") in
  let seeds =
    let parse s = String.split_on_char ' ' s |> List.map int_of_string in
    Scanf.sscanf (List.hd l) "seeds: %s@\n" parse
  in
  let maps =
    let parse_map str_l =
      let f d s l = (d, s, l) in
      List.map (fun s -> Scanf.sscanf s "%d %d %d" f) str_l
    in
    List.tl l
    |> List.map (fun s ->
           String.split_on_char '\n' s
           |> List.filter (fun s -> String.length s <> 0)
           |> List.tl)
    |> List.map parse_map
  in
  (seeds, maps)

(* Here is a way more elegant solution to solve part 1
   My own solution can be found just below *)

(*let rec apply_maps seeds maps =
  let apply seed = function
    | d, s, l when seed >= s && seed < s + l -> Some (seed - s + d)
    | _ -> None
  in
  let apply_map seeds map =
    List.map
      (fun s -> Option.value ~default:s (List.find_map (apply s) map))
      seeds
  in
  match maps with [] -> seeds | m :: ms -> apply_maps (apply_map seeds m) ms*)

let rec apply_maps seeds maps =
  let rec apply_map acc seeds ranges =
    match (seeds, ranges) with
    | seeds, [] -> seeds @ acc
    | [], _ :: ranges -> apply_map [] acc ranges
    | seed :: seeds, ((d, s, l) :: _ as ranges) ->
        if seed >= s && seed < s + l then
          (seed - s + d) :: apply_map acc seeds ranges
        else apply_map (seed :: acc) seeds ranges
  in
  match maps with
  | [] -> seeds
  | m :: ms -> apply_maps (apply_map [] seeds m) ms

let part_1 input =
  let seeds, maps = input in
  apply_maps seeds maps |> List.fold_left Int.min Int.max_int |> Answer.of_int

let part2_seeds seeds =
  let rec aux acc = function
    | start :: len :: t -> aux ((start, start + len - 1) :: acc) t
    | _ -> acc
  in
  List.rev (aux [] seeds)

let rec apply_maps seeds maps =
  let rec apply_map acc seeds ranges =
    match (seeds, ranges) with
    | _, [] -> seeds @ acc
    | [], _ :: ranges -> apply_map [] acc ranges
    | (s_s, s_e) :: seeds, ((_, s, _) :: _ as ranges) when s_e < s ->
        apply_map ((s_s, s_e) :: acc) seeds ranges
    | (s_s, s_e) :: seeds, ((_, s, l) :: _ as ranges) when s_s >= s + l ->
        apply_map ((s_s, s_e) :: acc) seeds ranges
    | (s_s, s_e) :: seeds, ((d, s, l) :: _ as ranges)
      when s_s >= s && s_e < s + l ->
        (s_s - s + d, s_e - s + d) :: apply_map acc seeds ranges
    | (s_s, s_e) :: seeds, ((_, s, _) :: _ as ranges) when s_s < s ->
        apply_map ((s_s, s - 1) :: acc) ((s, s_e) :: seeds) ranges
    | (s_s, s_e) :: seeds, ((d, s, l) :: _ as ranges) ->
        apply_map ((s_s - s + d, d + l) :: acc) ((s_e, s + l) :: seeds) ranges
  in
  match maps with
  | [] -> seeds
  | m :: ms -> apply_maps (apply_map [] seeds m) ms

let part_2 input =
  let seeds, maps = input in
  let seeds = part2_seeds seeds in
  apply_maps seeds maps |> List.map fst
  |> List.fold_left Int.min Int.max_int
  |> Answer.of_int
