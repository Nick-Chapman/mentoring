
let printf = Printf.printf

let () =
  printf "harmony...\n";
  let rec loop (acc0:float) n =
    let acc = acc0 +. (1. /. float n) in
    let bump = (truncate acc - truncate acc0) > 0 in
    let () = if bump then printf "%11d : %.30f\n%!" n acc in
    loop acc (n+1)
  in
  loop 0.0 2
