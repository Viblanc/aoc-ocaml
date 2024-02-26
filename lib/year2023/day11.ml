type input = char list list

let parse filename =
  Utils.read_file_as_lines filename
  |> List.map (fun s -> String.to_seq s |> List.of_seq)

let transpose l =
  let s = List.to_seq (List.map List.to_seq l) |> Seq.transpose in
  List.of_seq (Seq.map List.of_seq s)

let expand image =
  let rec expand = function
    | [] -> []
    | h :: t when List.for_all (Char.equal '.') h -> h :: h :: expand t
    | h :: t -> h :: expand t
  in
  expand image |> transpose |> expand

let get_galaxies image =
  List.mapi (fun i l -> List.mapi (fun j c -> ((i, j), c)) l) image
  |> List.flatten
  |> List.filter_map (fun (pos, c) -> if c = '#' then Some pos else None)

let shortest_path (x, y) (x', y') = Int.abs (x - x') + Int.abs (y - y')

let rec sum_paths galaxies =
  let rec aux galaxy = function
    | [] -> 0
    | g :: gs -> shortest_path galaxy g + aux galaxy gs
  in
  match galaxies with [] -> 0 | g :: gs -> aux g gs + sum_paths gs

let part_1 input = expand input |> get_galaxies |> sum_paths |> Answer.of_int

let get_galaxies image =
  let find_empty_lines l =
    List.mapi (fun i l -> (i, l)) l
    |> List.filter_map (fun (i, l) ->
           if List.for_all (Char.equal '.') l then Some i else None)
  in
  let galaxies = get_galaxies image in
  let empty_rows = find_empty_lines image in
  let empty_cols = find_empty_lines (transpose image) in
  List.map
    (fun (x, y) ->
      ( x
        + (1_000_000 - 1)
          * (List.filter (fun i -> i < x) empty_rows |> List.length),
        y
        + (1_000_000 - 1)
          * (List.filter (fun j -> j < y) empty_cols |> List.length) ))
    galaxies

let part_2 input = input |> get_galaxies |> sum_paths |> Answer.of_int
