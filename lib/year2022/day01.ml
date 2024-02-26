type input = int list list

let parse filename =
  Utils.read_file filename
  |> Str.split (Str.regexp "\n\n")
  |> List.map (fun s ->
         String.split_on_char '\n' s
         |> List.filter (fun s -> String.length s <> 0)
         |> List.map int_of_string)

let part_1 input =
  List.map (List.fold_left ( + ) 0) input
  |> List.fold_left max 0 |> Answer.of_int

let sum_first_n n l =
  let rec aux acc = function
    | 0, _ -> acc
    | _, [] -> failwith "should not happen"
    | n, h :: t -> aux (h + acc) (n - 1, t)
  in
  aux 0 (n, l)

let part_2 input =
  List.map (List.fold_left ( + ) 0) input
  |> List.sort (Fun.flip compare)
  |> sum_first_n 3 |> Answer.of_int
