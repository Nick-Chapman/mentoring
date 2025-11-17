
(* explore mutual recursion combinators *)

let printf = Printf.printf

(* direct single recursion *)
let () =
  let rec fact = fun n -> if n = 0 then 1 else n * fact (n-1) in
  let test n = printf "A: fact(%d) -> %d\n" n (fact n) in
  test 5

(* fixpoint combinator *)
module X : sig
  val fix : (('a->'b) -> ('a->'b)) -> ('a->'b)
end = struct
  let rec fix = fun f x -> f (fix f) x
end
include X

(* single recursion via fix combinator *)
let () =
  let open_fact = fun fact n -> if n = 0 then 1 else n * fact (n-1) in
  let fact = fix open_fact in
  let test n = printf "B: fact(%d) -> %d\n" n (fact n) in
  test 5

(* direct mutual recursion *)
let () =
  let rec odd = fun n -> if n = 0 then false else even (n-1)
  and even = fun n -> if n = 0 then true else odd (n-1) in
  let test n = printf "C: even(%d)->%b, odd(%d)->%b, \n" n (even n) n (odd n) in
  test 5

(* mutual fixpoint combinator *)
module XP : sig
  val fix_poly : (('a -> 'b) list -> ('a -> 'b)) list -> ('a -> 'b) list
end = struct
  let iota n =
    let rec loop i = if i < n then i :: loop (i+1) else []
    in loop 0
  let fix_poly tab =
    let n = List.length tab in
    let clause
      : (int -> 'a -> 'b) -> int -> 'a -> 'b
      = fun self i x -> (List.nth tab i) (List.map self (iota n)) x in
    let tied = fix clause in
    List.map tied (iota n)
end
include XP

(* improved mutual fixpoint combinator *)
module XP2 : sig
  val fix_poly : (('a -> 'b) list -> ('a -> 'b)) list -> ('a -> 'b) list
end = struct
  let fix_poly l =
    fix (fun self l -> List.map (fun li x -> li (self l) x) l) l
end
include XP2


(* mutual recursion via fix_poly combinator *)
let () =
  let open_odd = fun [_odd;even] n -> if n = 0 then false else even (n-1) in
  let open_even = fun [odd;_even] n -> if n = 0 then true else odd (n-1) in
  let [odd;even] = fix_poly [open_odd;open_even] in
  let test n = printf "D: even(%d)->%b, odd(%d)->%b, \n" n (even n) n (odd n) in
  test 5

let () = printf "done\n"
