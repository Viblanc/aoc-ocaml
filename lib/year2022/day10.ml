type input = int option list

module M = Map.Make (Int)

let parse filename =
  let parse_line line =
    match String.split_on_char ' ' line with
    | [ "addx"; n ] -> Some (int_of_string n)
    | _ -> None
  in
  Utils.read_file_as_lines filename |> List.map parse_line

let rec run_cycles n signals instructions =
  let x = Option.value ~default:1 (M.find_opt (n - 1) signals) in
  let nxt = x + Option.value ~default:0 (M.find_opt n signals) in
  match instructions with
  | [] -> signals
  | None :: l -> run_cycles (n + 1) (M.add n nxt signals) l
  | Some i :: l ->
      run_cycles (n + 2)
        M.(add n nxt (add (n + 1) nxt (add (n + 2) i signals)))
        l

let part_1 input =
  let map = run_cycles 1 M.empty input in
  List.map (fun i -> i * M.find i map) [ 20; 60; 100; 140; 180; 220 ]
  |> List.fold_left ( + ) 0 |> Answer.of_int

let lit_pixel (cycle, x) =
  let pixel = (cycle - 1) mod 40 in
  if pixel >= x - 1 && pixel <= x + 1 then '#' else '.'

let part_2 input =
  let rec split seq =
    match seq () with
    | Seq.Nil -> []
    | _ -> Seq.take 40 seq :: split (Seq.drop 40 seq)
  in
  let map = run_cycles 1 M.empty input in
  M.to_seq map |> Seq.map lit_pixel |> split
  |> List.map (fun seq -> String.of_seq seq ^ "\n")
  |> List.fold_left ( ^ ) "" |> Answer.of_string
