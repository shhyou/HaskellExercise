exception Undefined;;

let paren b s = if b then "(" ^ s ^ ")" else s

module STLC : sig
  type expr = VAR of string
            | LAM of string * typ * expr
            | AP  of expr * expr
            | LET of string * expr * expr

  and typ = TVAR of string
          | TARR of typ * typ

  val expr_to_string : expr -> string
  val typ_to_string : typ -> string
  val typecheck : expr * typ -> bool
  val test : unit -> unit
end =
struct
  type expr = VAR of string
            | LAM of string * typ * expr
            | AP  of expr * expr
            | LET of string * expr * expr

  and typ = TVAR of string
          | TARR of typ * typ

  let (expr_to_string, typ_to_string) =
    let rec showe = function
        d, VAR x -> x
      | d, LAM (x, t, e) -> paren (d > 0) ("\\(" ^ x ^ ":" ^ showt (0, t) ^ "). " ^ showe (0, e))
      | d, AP (e1, e2) -> paren (d > 10) (showe (10, e1) ^ " " ^ showe (11, e2))
      | d, LET (x, e1, e2) -> paren (d > 0) ("let " ^ x ^ " = " ^ showe (0, e1) ^ " in " ^ showe (0, e2))
    and showt = function
        d, TVAR a -> a
      | d, TARR (t1, t2) -> paren (d > 0) (showt (1, t1) ^ " -> " ^ showt (0, t2)) in
    ((fun e -> showe (0, e)), (fun t -> showt (0, t)))

  let typecheck =
    let rec infer = function
        cxt, VAR x -> Some (List.assoc x cxt)
      | cxt, LAM (x, t1, e) ->
         (match infer ((x,t1)::cxt, e) with
           Some t2 -> Some (TARR (t1, t2))
         | None -> None)
      | cxt, AP (e1, e2) ->
         (match (infer (cxt, e1), infer (cxt, e2)) with
           (Some (TARR (t1, t2)), Some t1') when t1 = t1' -> Some t2
         | _ -> None)
      | cxt, LET (x, e1, e2) ->
         (match infer (cxt, e1) with
           Some t -> infer ((x,t)::cxt, e2)
         | _ -> None) in
      fun (e, t) -> match infer ([], e) with Some t' -> t = t' | None -> false

  (* \(x:A). x *)
  let e0 = LAM ("x", TVAR "A", VAR "x")
  let t0 = TARR (TVAR "A", TVAR "A")

  (* \(f:A -> A -> B). \(x:A). f x x *)
  let e1 = LAM ("f", TARR (TVAR "A", TARR (TVAR "A", TVAR "B")),
            LAM ("x", TVAR "A",
              AP (AP (VAR "f", VAR "x"), VAR "x")))
  let t1 = TARR (TARR (TVAR "A", TARR (TVAR "A", TVAR "B")),
                 TARR (TVAR "A", TVAR "B"))

  (* \(f:A -> A -> A). let g = \(x:A). f x x in \(y:A). f (g y) (g y) *)
  let e2 = LAM ("f", TARR (TVAR "A", TARR (TVAR "A", TVAR "A")),
            LET ("g", LAM ("x", TVAR "A",
                        AP (AP (VAR "f", VAR "x"), VAR "x")),
              LAM ("y", TVAR "A",
                    AP (AP (VAR "f", AP (VAR "g", VAR "y")), AP (VAR "g", VAR "y")))))
  let t2 = TARR (TARR (TVAR "A", TARR (TVAR "A", TVAR "A")),
                 TARR (TVAR "A", TVAR "A"))

  let test () =
    let test_check (e, t) = begin
      print_string ("Check " ^ expr_to_string e ^ " with " ^ typ_to_string t);
      print_endline (" : " ^ (match typecheck (e, t) with true -> "true" | false -> "false"))
    end in
    begin
      test_check (e0, t0);
      test_check (e1, t1);
      test_check (e2, t2)
    end
end

module LC : sig
  type expr = VAR of string
            | LAM of string * expr
            | AP  of expr * expr
            | LET of string * expr * expr

  val expr_to_string : expr -> string
  val typeinfer : expr -> STLC.typ
end =
struct
  type expr = VAR of string
            | LAM of string * expr
            | AP  of expr * expr
            | LET of string * expr * expr

  let expr_to_string =
    let rec showe = function
        d, VAR x -> x
      | d, LAM (x, e) -> paren (d > 0) ("\\" ^ x ^ ". " ^ showe (0, e))
      | d, AP (e1, e2) -> paren (d > 10) (showe (10, e1) ^ " " ^ showe (11, e2))
      | d, LET (x, e1, e2) -> paren (d > 0) ("let " ^ x ^ " = " ^ showe (0, e1) ^ " in " ^ showe (0, e2)) in
    fun e -> showe (0, e)

  (* meta type *)
  type typ = TVAR of metavar ref
           | TARR of typ * typ
  and metavar = UNLINK of string | LINK of typ

  let concretize_type (t : typ) : STLC.typ = raise Undefined

  let typeinfer =
    let fresh_sym =
      let cnt = ref 0 in
      fun () -> begin
        cnt := !cnt + 1;
        "t" ^ string_of_int (!cnt)
      end in

    let add_equation (t1, t2) = raise Undefined in

    let rec gen_cons = function
        cxt, VAR x -> List.assoc x cxt
      | cxt, LAM (x, e) ->
          let t = TVAR (ref (UNLINK (fresh_sym ()))) in
          let t' = gen_cons ((x, t)::cxt, e) in
          TARR (t, t')
      | cxt, AP (e1, e2) ->
          let t1 = gen_cons (cxt, e1) in
          let t2 = gen_cons (cxt, e2) in
          let t = TVAR (ref (UNLINK (fresh_sym ()))) in
          (add_equation (t1, TARR (t2, t));
           t)
      | cxt, LET (x, e1, e2) -> gen_cons ((x, gen_cons (cxt, e1))::cxt, e2) in
    fun e -> raise Undefined

  type typescheme = MONO of typ
                  | POLY of string list * typ
  let typeinfer_hm e = raise Undefined
end

module SystemF : sig
  type expr = VAR  of string 
            | LAM  of string * typ * expr
            | AP   of expr * expr
            | TLAM of string * expr
            | TAP  of expr * typ

  and typ = TVAR of string
          | TARR of typ * typ
          | TALL of string * typ
end =
struct
  type expr = VAR  of string 
            | LAM  of string * typ * expr
            | AP   of expr * expr
            | TLAM of string * expr
            | TAP  of expr * typ

  and typ = TVAR of string
          | TARR of typ * typ
          | TALL of string * typ
end
