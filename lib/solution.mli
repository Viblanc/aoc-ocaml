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

module Solve (_ : Year) : S
