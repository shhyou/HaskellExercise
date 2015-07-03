exception Undefined;;

let paren b s = if b then "(" ^ s ^ ")" else s

(* remove duplicated terms, O(n^2) *)
let nub =
  let rec remove = function
      cxt, [] -> []
    | cxt, x::xs -> if List.mem x cxt then remove (cxt, xs) else x::remove (x::cxt, xs) in
  fun xs -> remove ([], xs)

(* xs \\ ys *)
let diff xs ys = List.filter (fun x -> not (List.mem x ys)) xs;;

#use "stlc.ml"
#use "sysf.ml"
#use "lc.ml"
#use "bidir.ml"
#use "infer.ml"
