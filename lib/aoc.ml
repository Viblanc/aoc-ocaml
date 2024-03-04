module Solution = Solution

module Year2022 = struct
  include Year2022

  let year = 2022

  let days =
    [|
      (module Year2022.Day01 : Solution.Day);
      (module Year2022.Day02 : Solution.Day);
      (module Year2022.Day03 : Solution.Day);
      (module Year2022.Day04 : Solution.Day);
      (module Year2022.Day05 : Solution.Day);
      (module Year2022.Day06 : Solution.Day);
      (module Year2022.Day07 : Solution.Day);
      (module Year2022.Day08 : Solution.Day);
      (module Year2022.Day09 : Solution.Day);
      (module Year2022.Day10 : Solution.Day);
      (module Year2022.Day11 : Solution.Day);
      (module Year2022.Day12 : Solution.Day);
    |]
end

module Year2023 = struct
  include Year2023

  let year = 2023

  let days =
    [|
      (module Year2023.Day01 : Solution.Day);
      (module Year2023.Day02 : Solution.Day);
      (module Year2023.Day03 : Solution.Day);
      (module Year2023.Day04 : Solution.Day);
      (module Year2023.Day05 : Solution.Day);
      (module Year2023.Day06 : Solution.Day);
      (module Year2023.Day07 : Solution.Day);
      (module Year2023.Day08 : Solution.Day);
      (module Year2023.Day09 : Solution.Day);
      (module Year2023.Day10 : Solution.Day);
      (module Year2023.Day11 : Solution.Day);
      (module Year2023.Day12 : Solution.Day);
      (module Year2023.Day13 : Solution.Day);
      (module Year2023.Day14 : Solution.Day);
      (module Year2023.Day15 : Solution.Day);
      (module Year2023.Day16 : Solution.Day);
      (module Year2023.Day17 : Solution.Day);
      (module Year2023.Day18 : Solution.Day);
      (module Year2023.Day19 : Solution.Day);
      (module Year2023.Day20 : Solution.Day);
      (module Year2023.Day21 : Solution.Day);
    |]
end

module Answer = Answer
