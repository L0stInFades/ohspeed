let mean values =
  match values with
  | [] -> None
  | _ ->
      let sum = List.fold_left ( +. ) 0. values in
      Some (sum /. float_of_int (List.length values))

let percentile values p =
  match values with
  | [] -> None
  | _ ->
      let sorted = List.sort Float.compare values |> Array.of_list in
      let n = Array.length sorted in
      if n = 1 then
        Some sorted.(0)
      else
        let p = Float.max 0. (Float.min 1. p) in
        let position = p *. float_of_int (n - 1) in
        let lower = int_of_float (Float.floor position) in
        let upper = int_of_float (Float.ceil position) in
        if lower = upper then
          Some sorted.(lower)
        else
          let weight = position -. float_of_int lower in
          Some
            ((sorted.(lower) *. (1. -. weight)) +. (sorted.(upper) *. weight))

let jitter values =
  match values with
  | [] | [ _ ] -> None
  | first :: rest ->
      let total, count, _ =
        List.fold_left
          (fun (sum, samples, previous) current ->
            (sum +. Float.abs (current -. previous), samples + 1, current))
          (0., 0, first) rest
      in
      Some (total /. float_of_int count)

let take_last count values =
  let rec keep remaining acc = function
    | [] -> acc
    | head :: tail ->
        if remaining <= 0 then
          keep 0 acc tail
        else
          keep (remaining - 1) (head :: acc) tail
  in
  if count <= 0 then
    []
  else
    let rev_values = List.rev values in
    keep count [] rev_values

