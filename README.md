# Advent of Code in OCaml

Here's my attempt at solving every puzzle from every year of AoC in OCaml.  
I am by no means an expert in OCaml, so this should serve as a nice learning experience as well.

## Getting started

Place your inputs in a directory at the root of the project called `inputs/{year}`.  
Each input is named accordingly to their associated day. For day 1 of 2023, the input path will be `inputs/2023/1.txt`.  
For day 14 of 2016, `inputs/2016/14.txt`, etc.  
My inputs are not present as the author of Advent of Code explicitly stated he did not wish for those inputs to be shared.  

To run a solution, do :  
`dune exec -- {year} -day {day}`

You can run everything by just specifying the year :  
`dune exec {year}`

Tests can be run with `dune test`.  
If you wish to run the tests for an individual year, run :  
`dune test ./test/{year}`
