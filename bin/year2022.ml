open Aoc
module Solve2022 = Solution.Solve (Year2022)

let rec show_solution day =
  match day with
  | 0 -> List.init 25 succ |> List.iter show_solution
  | n when n > 25 || n < 0 -> failwith "Not a valid day"
  | n -> Solve2022.print n

let () =
  let day = ref 0 in
  let speclist = [ ("-day", Arg.Set_int day, "Day of AoC") ] in
  let usagemsg = "AoC 2022" in
  Arg.parse speclist print_endline usagemsg;
  show_solution !day
