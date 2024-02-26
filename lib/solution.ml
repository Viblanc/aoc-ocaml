module type Day = sig
  type input

  val parse : string -> input
  val part_1 : input -> Answer.t
  val part_2 : input -> Answer.t
end

module type Year = sig
  val year : int
  val days : (module Day) array
end

module type S = sig
  val solve_part1 : int -> string -> Answer.t
  val solve_part2 : int -> string -> Answer.t
  val print : int -> unit
end

module Solve (Sol : Year) = struct
  let solve_part1 day filename =
    let idx = day - 1 in
    let (module D) = Sol.days.(idx) in
    let parsed_input = D.parse filename in
    D.part_1 parsed_input

  let solve_part2 day filename =
    let idx = day - 1 in
    let (module D) = Sol.days.(idx) in
    let parsed_input = D.parse filename in
    D.part_2 parsed_input

  let print day =
    let idx = day - 1 in
    let (module D) = Sol.days.(idx) in
    let filename = Printf.sprintf "./inputs/%d/%d.txt" Sol.year day in
    let parsed_input = D.parse filename in
    let p1, p2 = (D.part_1 parsed_input, D.part_2 parsed_input) in
    let print_sol i a =
      Format.printf "Solution for part %d: " i;
      Format.printf "%a\n" Answer.pp_answer a
    in
    Format.printf "Solutions for day %d:\n" day;
    print_sol 1 p1;
    print_sol 2 p2
end
