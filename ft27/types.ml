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

module SystemF : sig
  type expr = VAR  of string
            | LAM  of string * typ * expr
            | AP   of expr * expr
            | LET  of string * expr * expr
            | TLAM of string * expr
            | TAP  of expr * typ

  and typ = TVAR of string
          | TARR of typ * typ
          | TALL of string * typ

  val expr_to_string : expr -> string
  val typ_to_string : typ -> string
  val test : unit -> unit
end =
struct
  type expr = VAR  of string
            | LAM  of string * typ * expr
            | AP   of expr * expr
            | LET  of string * expr * expr
            | TLAM of string * expr
            | TAP  of expr * typ

  and typ = TVAR of string
          | TARR of typ * typ
          | TALL of string * typ

  let (expr_to_string, typ_to_string) =
    let rec showe = function
        d, VAR x -> x
      | d, LAM (x, t, e) -> paren (d > 0) ("\\(" ^ x ^ ":" ^ showt (0, t) ^ "). " ^ showe (0, e))
      | d, AP (e1, e2) -> paren (d > 10) (showe (10, e1) ^ " " ^ showe (11, e2))
      | d, LET (x, e1, e2) -> paren (d > 0) ("let " ^ x ^ " = " ^ showe (0, e1) ^ " in " ^ showe (0, e2))
      | d, TLAM (a, e) -> paren (d > 0) ("/\\" ^ a ^ ". " ^ showe (0, e))
      | d, TAP (e, t) -> paren (d > 10) (showe (10, e) ^ " [" ^ showt (0, t) ^ "]")
    and showt = function
        d, TVAR a -> a
      | d, TARR (t1, t2) -> paren (d > 0) (showt (1, t1) ^ " -> " ^ showt (0, t2))
      | d, TALL (a, t) -> paren (d > 0) ("\\/" ^ a ^ ". " ^ showt (0, t)) in
    ((fun e -> showe (0, e)), (fun t -> showt (0, t)))

  (* /\A. \(x:A). x *)
  let e0 = TLAM ("A", LAM ("x", TVAR "A", VAR "x"))
  let t0 = TALL ("A", TARR (TVAR "A", TVAR "A"))

  let e3 = LET ("id", TLAM ("A", LAM ("x", TVAR "A", VAR "x")),
                AP (TAP (VAR "id", TALL ("B", TARR (TVAR "B", TVAR "B"))), VAR "id"))
  let t3 = TALL ("B", TARR (TVAR "B", TVAR "B"))

  let test () = begin
    print_endline (expr_to_string e0 ^ " : " ^ typ_to_string t0);
    print_endline (expr_to_string e3 ^ " : " ^ typ_to_string t3)
  end
end

module LC : sig
  type expr = VAR of string
            | LAM of string * expr
            | AP  of expr * expr
            | LET of string * expr * expr

  val expr_to_string : expr -> string
  val typeinfer : expr -> STLC.expr * STLC.typ
  val test : unit -> unit
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
  (* code reference: http://okmij.org/ftp/ML/generalization.html *)
  type typ = TVAR of metavar ref
           | TARR of typ * typ
  and metavar = UNLINK of string | LINK of typ

  let rec concretize_type = function
      TVAR {contents = UNLINK x} -> STLC.TVAR x
    | TVAR {contents = LINK t} -> concretize_type t
    | TARR (t1, t2) -> STLC.TARR (concretize_type t1, concretize_type t2)

  let typeinfer expr =
    let fresh_sym =
      let cnt = ref 0 in
      fun () -> begin
        cnt := !cnt + 1;
        "t" ^ string_of_int (!cnt)
      end in

    let equations = ref [] in

    let add_equation (eq : typ * typ) = equations := eq::(!equations) in

    let print_equations () =
      List.iter
        (fun (t1, t2) ->
          print_endline (STLC.typ_to_string (concretize_type t1) ^ " = "
                       ^ STLC.typ_to_string (concretize_type t2)))
        (!equations) in

    let solve_equations () =
      let rec occurs e = false in
      let rec unify = function
          TVAR {contents = LINK t}, t' | t', TVAR {contents = LINK t} -> unify (t, t')
        | TVAR ({contents = UNLINK x}), TVAR ({contents = UNLINK x'}) when x = x' ->
            ()
        | TVAR ({contents = UNLINK x} as r), t | t, TVAR ({contents = UNLINK x} as r) ->
            r := LINK t
        | TARR (t1, t2), TARR (t1', t2') -> (unify (t1, t1'); unify (t2, t2')) in
      List.iter unify (!equations) in

    let rec gen_cons = function
        cxt, VAR x -> (fun () -> STLC.VAR x), List.assoc x cxt
      | cxt, LAM (x, e) ->
          let t = TVAR (ref (UNLINK (fresh_sym ()))) in
          let e', t' = gen_cons ((x, t)::cxt, e) in
          (fun () -> STLC.LAM (x, concretize_type t, e' ())), TARR (t, t')
      | cxt, AP (e1, e2) ->
          let e1', t1 = gen_cons (cxt, e1) in
          let e2', t2 = gen_cons (cxt, e2) in
          let t = TVAR (ref (UNLINK (fresh_sym ()))) in
          (add_equation (t1, TARR (t2, t));
           (fun () -> STLC.AP (e1' (), e2' ())), t)
      | cxt, LET (x, e1, e2) ->
          let e1', t = gen_cons (cxt, e1) in
          let e2', t' = gen_cons ((x, t)::cxt, e2) in
          (fun () -> STLC.LET (x, e1' (), e2' ())), t' in

    let expr', t = gen_cons ([], expr) in
    begin
      print_equations ();
      solve_equations ();
      (expr' (), concretize_type t)
    end

  type typescheme = MONO of typ
                  | POLY of string list * typ
  let typeinfer_hm e = raise Undefined

  (* \x. x *)
  let e0 = LAM ("x", VAR "x")

  (* \f. \x. f x x *)
  let e1 = LAM ("f",
            LAM ("x",
              AP (AP (VAR "f", VAR "x"), VAR "x")))

  (* \f. let g = \x. f x x in \y. f (g y) (g y) *)
  let e2 = LAM ("f",
            LET ("g", LAM ("x",
                        AP (AP (VAR "f", VAR "x"), VAR "x")),
              LAM ("y",
                    AP (AP (VAR "f", AP (VAR "g", VAR "y")), AP (VAR "g", VAR "y")))))

  let test () = begin
    let test_mono e = begin
      print_endline ("Inferring " ^ expr_to_string e);
      let e', t = typeinfer e in
      print_endline ("  (" ^ STLC.expr_to_string e' ^ ") : " ^ STLC.typ_to_string t)
    end in
    test_mono e0;
    test_mono e1;
    test_mono e2
  end
end
