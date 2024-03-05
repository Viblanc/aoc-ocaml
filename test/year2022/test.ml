open Alcotest
open Aoc

let answer = Alcotest.testable Answer.pp_answer Answer.eq

module Solver = Solution.Solve (Year2022)

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
    ("Day 1", `Quick, test_day 1 (Answer.Int 24000, Answer.Int 45000));
    ("Day 2", `Quick, test_day 2 (Answer.Int 15, Answer.Int 12));
    ("Day 3", `Quick, test_day 3 (Answer.Int 157, Answer.Int 70));
    ("Day 4", `Quick, test_day 4 (Answer.Int 2, Answer.Int 4));
    ("Day 5", `Quick, test_day 5 (Answer.Str "CMZ", Answer.Str "MCD"));
    ("Day 6", `Quick, test_day 6 (Answer.Int 11, Answer.Int 26));
    ("Day 7", `Quick, test_day 7 (Answer.Int 95437, Answer.Int 24933642));
    ("Day 8", `Quick, test_day 8 (Answer.Int 21, Answer.Int 8));
    ("Day 9", `Quick, test_day 9 ~two:true (Answer.Int 13, Answer.Int 36));
    ( "Day 10",
      `Quick,
      test_day 10
        ( Answer.Int 13140,
          Answer.Str
            "##..##..##..##..##..##..##..##..##..##..\n\
             ###...###...###...###...###...###...###.\n\
             ####....####....####....####....####....\n\
             #####.....#####.....#####.....#####.....\n\
             ######......######......######......####\n\
             #######.......#######.......#######.....\n" ) );
    ("Day 11", `Quick, test_day 11 (Answer.Int 10605, Answer.Int 2713310158));
    ("Day 12", `Quick, test_day 12 (Answer.Int 31, Answer.Int 29));
    ("Day 13", `Quick, test_day 13 (Answer.Int 13, Answer.Int 140));
  ]

let () = Alcotest.run "Advent of Code" [ ("Year 2022", suite) ]
