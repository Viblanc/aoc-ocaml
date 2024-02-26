type spring = O | D | U
type input = (spring list * int list) list

let spring_of_char = function '.' -> O | '#' -> D | _ -> U

let parse_springs lines =
  let parse_row row groups =
    let row = String.to_seq row |> List.of_seq |> List.map spring_of_char in
    let groups = String.split_on_char ',' groups |> List.map int_of_string in
    (row, groups)
  in
  List.map (fun s -> Scanf.sscanf s "%s %s@\n" parse_row) lines

let count_arrangements (springs, groups) =
  let rec aux pre (springs, groups) =
    if List.length springs < List.fold_left ( + ) 0 groups then 0
    else
      match (springs, groups) with
      | [], [] -> 1
      | [], [ 0 ] -> 1
      | [], _ -> 0
      | D :: _, [] -> 0
      | O :: ss, [] -> aux O (ss, [])
      | O :: ss, 0 :: gs -> aux O (ss, gs)
      | O :: ss, _ -> if pre = D then 0 else aux O (ss, groups)
      | D :: _, 0 :: _ -> 0
      | D :: ss, n :: gs -> aux D (ss, (n - 1) :: gs)
      | U :: ss, _ -> aux pre (O :: ss, groups) + aux pre (D :: ss, groups)
  in
  aux O (springs, groups)

let parse filename = Utils.read_file_as_lines filename |> parse_springs

let part_1 input =
  List.fold_left (fun acc l -> acc + count_arrangements l) 0 input
  |> Answer.of_int

let count_arrangements (springs, groups) =
  let mem = Hashtbl.create 2048 in
  let rec aux pre (springs, groups) =
    match Hashtbl.find_opt mem (springs, groups) with
    | Some n -> n
    | None ->
        let n =
          match (springs, groups) with
          | [], [] -> 1
          | [], [ 0 ] -> 1
          | [], _ -> 0
          | D :: _, [] -> 0
          | O :: ss, [] -> aux O (ss, [])
          | O :: ss, 0 :: gs -> aux O (ss, gs)
          | O :: ss, _ -> if pre = D then 0 else aux O (ss, groups)
          | D :: _, 0 :: _ -> 0
          | D :: ss, n :: gs -> aux D (ss, (n - 1) :: gs)
          | U :: ss, _ -> aux pre (O :: ss, groups) + aux pre (D :: ss, groups)
        in
        Hashtbl.add mem (springs, groups) n;
        n
  in
  aux O (springs, groups)

let correct_input (springs, groups) =
  ( springs :: (Seq.repeat (U :: springs) |> Seq.take 4 |> List.of_seq)
    |> List.flatten,
    Seq.repeat groups |> Seq.take 5 |> List.of_seq |> List.flatten )

let part_2 input =
  List.map correct_input input
  |> List.fold_left (fun acc l -> acc + count_arrangements l) 0
  |> Answer.of_int
