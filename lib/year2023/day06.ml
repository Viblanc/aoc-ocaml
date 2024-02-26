type input = (int * int) list

let parse filename =
  let lines = Utils.read_file_as_lines filename in
  let times = List.hd lines |> Utils.find_numbers in
  let distances = List.tl lines |> List.hd |> Utils.find_numbers in
  List.combine times distances

let can_win speed (race_time, record_dist) =
  let time_left = race_time - speed in
  let dist_traveled = speed * time_left in
  dist_traveled > record_dist

let find_ways_to_win race =
  let speeds = List.init (fst race + 1) Fun.id in
  List.map (fun s -> Bool.to_int (can_win s race)) speeds
  |> List.fold_left ( + ) 0

let part_1 input =
  List.map find_ways_to_win input |> List.fold_left ( * ) 1 |> Answer.of_int

(* A more time-efficient way would be to find the first and last values for which
   the race record can still be beaten *)
(* Another fast way would be to solve a quadratic equation *)

let find_ways_to_win race =
  let t, _ = race in
  let rec aux speed f = if can_win speed race then speed else aux (f speed) f in
  let first = aux 0 Int.succ in
  let last = aux t Int.pred in
  last - first + 1

let concat_strings l =
  List.map string_of_int l |> List.fold_left ( ^ ) "" |> int_of_string

let part_2 input =
  let times, dists = List.split input in
  let time, dist = (concat_strings times, concat_strings dists) in
  find_ways_to_win (time, dist) |> Answer.of_int
