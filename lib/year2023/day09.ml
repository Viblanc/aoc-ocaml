type input = int list list

let parse filename =
  Utils.read_file_as_lines filename
  |> List.map (fun s -> String.split_on_char ' ' s |> List.map int_of_string)

let rec windows = function
  | [ a; b ] -> [ (a, b) ]
  | a :: (b :: _ as l) -> (a, b) :: windows l
  | _ -> failwith "Should not happen!"

let rec gen_differences = function
  | [ a; b ] -> [ b - a ]
  | a :: (b :: _ as l) -> (b - a) :: gen_differences l
  | _ -> failwith "Should not happen!"

let extrapolate_next_value l =
  let rec aux acc l =
    match List.for_all (Int.equal 0) l with
    | true -> acc
    | false -> aux ((List.rev l |> List.hd) :: acc) (gen_differences l)
  in
  aux [] l |> List.fold_left ( + ) 0

let part_1 input =
  List.map extrapolate_next_value input
  |> List.fold_left ( + ) 0 |> Answer.of_int

let extrapolate_prev_value l =
  let rec aux acc l =
    match List.for_all (Int.equal 0) l with
    | true -> List.rev acc
    | false -> aux (List.hd l :: acc) (gen_differences l)
  in
  List.fold_right ( - ) (aux [] l) 0

let part_2 input =
  List.map extrapolate_prev_value input
  |> List.fold_left ( + ) 0 |> Answer.of_int
