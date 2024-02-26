open Alcotest
open Aoc

let answer = Alcotest.testable Answer.pp_answer Answer.eq

module Solver = Solution.Solve (Year2023)

let test_day day ?(two = false) want () =
  match two with
  | false ->
      let filename = Printf.sprintf "./examples/%d.txt" day in
      let p1, p2 =
        (Solver.solve_part1 day filename, Solver.solve_part2 day filename)
      in
      check (pair answer answer) "same result" want (p1, p2)
  | true ->
      let f1 = Printf.sprintf "./examples/%d_1.txt" day in
      let f2 = Printf.sprintf "./examples/%d_2.txt" day in
      let p1 = Solver.solve_part1 day f1 in
      let p2 = Solver.solve_part2 day f2 in
      check (pair answer answer) "same result" want (p1, p2)

let suite =
  [
    ("Day 1", `Quick, test_day 1 ~two:true (Answer.Int 142, Answer.Int 281));
    ("Day 2", `Quick, test_day 2 (Answer.Int 8, Answer.Int 2286));
    ("Day 3", `Quick, test_day 3 (Answer.Int 4361, Answer.Int 467835));
    ("Day 4", `Quick, test_day 4 (Answer.Int 13, Answer.Int 30));
    ("Day 5", `Quick, test_day 5 (Answer.Int 35, Answer.Int 46));
    ("Day 6", `Quick, test_day 6 (Answer.Int 288, Answer.Int 71503));
    ("Day 7", `Quick, test_day 7 (Answer.Int 6440, Answer.Int 5905));
    ("Day 8", `Quick, test_day 8 ~two:true (Answer.Int 6, Answer.Int 6));
    ("Day 9", `Quick, test_day 9 (Answer.Int 114, Answer.Int 2));
    ("Day 10", `Quick, test_day 10 ~two:true (Answer.Int 8, Answer.Int 10));
    ("Day 11", `Quick, test_day 11 (Answer.Int 374, Answer.Int 82000210));
    ("Day 12", `Quick, test_day 12 (Answer.Int 21, Answer.Int 525152));
    ("Day 13", `Quick, test_day 13 (Answer.Int 405, Answer.Int 400));
    ("Day 14", `Quick, test_day 14 (Answer.Int 136, Answer.Int 64));
    ("Day 15", `Quick, test_day 15 (Answer.Int 1320, Answer.Int 145));
    ("Day 16", `Quick, test_day 16 (Answer.Int 46, Answer.Int 51));
    (*     ("Day 17", `Quick, test_day (module Year2023.Day17) 17 (102, 94)); *)
    ("Day 18", `Quick, test_day 18 (Answer.Int 62, Answer.Int 952408144115));
    (* ( "Day 19",
       `Quick,
       test_day
         (module Year2023.Day19)
         19
         (Answer.Int 19114, Answer.Int 167409079868000) );*)
    ("Day 21", `Quick, test_day 21 (Answer.Int 42, Answer.Int 0));
  ]

let () = Alcotest.run "Advent of Code" [ ("Year 2023", suite) ]
