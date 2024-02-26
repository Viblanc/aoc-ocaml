type input = ((int * int) * (int * int)) list

let parse filename =
  let scan_line a b c d = ((a, b), (c, d)) in
  Utils.read_file_as_lines filename
  |> List.map (fun s -> Scanf.sscanf s "%d-%d,%d-%d" scan_line)

let part_1 input =
  List.filter
    (fun ((a, b), (c, d)) -> (a >= c && b <= d) || (a <= c && b >= d))
    input
  |> List.length |> Answer.of_int

let part_2 input =
  List.filter (fun ((a, b), (c, d)) -> not (b < c || a > d)) input
  |> List.length |> Answer.of_int
