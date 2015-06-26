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
  val get_type : expr -> typ option
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
    (fun e -> showe (0, e)), (fun t -> showt (0, t))

  let get_type =
    let rec infer = function
        cxt, VAR x -> Some (List.assoc x cxt)
      | cxt, LAM (x, t1, e) ->
         (match infer ((x,t1)::cxt, e) with
           Some t2 -> Some (TARR (t1, t2))
         | None -> None)
      | cxt, AP (e1, e2) ->
         (match infer (cxt, e1), infer (cxt, e2) with
           Some (TARR (t1, t2)), Some t1' when t1 = t1' -> Some t2
         | _ -> None)
      | cxt, LET (x, e1, e2) ->
         (match infer (cxt, e1) with
           Some t -> infer ((x,t)::cxt, e2)
         | _ -> None) in
      fun e -> infer ([], e)

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
      print_endline ("Check " ^ expr_to_string e ^ "\n  " ^ typ_to_string t);
      print_endline ("  " ^ (match get_type e with Some t' -> typ_to_string t' | None -> "None") ^ "\n")
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
  val type_check : string list * typ -> bool
  val type_equal : typ * typ -> bool
  val type_subst : typ * string * typ -> typ
  val get_type : expr -> typ option
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

  (* let id = /\A. \(x:A). x in id [\/B. B -> B] id *)
  let e3 = LET ("id", TLAM ("A", LAM ("x", TVAR "A", VAR "x")),
                AP (TAP (VAR "id", TALL ("B", TARR (TVAR "B", TVAR "B"))), VAR "id"))
  let t3 = TALL ("B", TARR (TVAR "B", TVAR "B"))

  let rec type_check = function
      tcxt, TVAR x -> List.mem x tcxt
    | tcxt, TARR (t1, t2) -> type_check (tcxt, t1) && type_check (tcxt, t2)
    | tcxt, TALL (a, t) -> type_check (a::tcxt, t)

  let type_equal =
    let rec check = function
        cxt1, cxt2, map, TVAR x, TVAR y ->
          let bound1, bound2 = List.mem x cxt1, List.mem x cxt2 in
          bound1 == bound2 && (not bound1 || List.assoc x map == y)
      | cxt1, cxt2, map, TARR (t1, t2), TARR (t1', t2') ->
          check (cxt1, cxt2, map, t1, t1') && check (cxt1, cxt2, map, t2, t2')
      | cxt1, cxt2, map, TALL (a, t), TALL (a', t') ->
          check (a::cxt1, a'::cxt2, (a,a')::map, t, t')
      | _ -> false in
    fun (t1, t2) -> check ([], [], [], t1, t2)

  (* type_subst (t, a, t') := t [t' / a], substitute t' for a in t *)
  let type_subst =
    let fresh_sym =
      let cnt = ref 0 in
      fun () -> begin
        cnt := !cnt + 1;
        "t" ^ string_of_int (!cnt)
      end in

    (* alpha conversion; alpha_conv (t, y, x) = t|_{x = y} *)
    let rec alpha_conv = function
        TVAR a as t, y, x -> if a = x then TVAR y else t
      | TARR (t1, t2), y, x -> TARR (alpha_conv (t1, y, x), alpha_conv (t2, y, x))
      | TALL (a, _) as t, y, x when a = x -> t
      | TALL (a, t), y, x -> TALL (a, alpha_conv (t, y, x)) in

    let rec subst = function
        TVAR x as t, a, t' -> if x = a then t' else t
      | TARR (t1, t2), a, t' -> TARR (subst (t1, a, t'), subst (t2, a, t'))
      | TALL (x, _) as t, a, t' when x = a -> t
      | TALL (x, t), a, t' ->
          let y = fresh_sym () in
          TALL (y, subst (t, a, alpha_conv (t', y, x))) in
    subst

  let get_type =
    let rec infer = function
        tcxt, ecxt, VAR x -> Some (List.assoc x ecxt)
      | tcxt, ecxt, LAM (x, t, e) ->
          if not (type_check (tcxt, t))
            then None
            else (match infer (tcxt, (x,t)::ecxt, e) with
                    Some t' -> Some (TARR (t, t'))
                  | None -> None)
      | tcxt, ecxt, AP (e1, e2) ->
          (match infer (tcxt, ecxt, e1), infer (tcxt, ecxt, e2) with
            Some (TARR (t1, t2)), Some t1' when type_equal (t1, t1') -> Some t2
          | _ -> None)
      | tcxt, ecxt, LET (x, e1, e2) ->
          (match infer (tcxt, ecxt, e1) with
            Some t -> infer (tcxt, (x,t)::ecxt, e2)
          | None -> None)
      | tcxt, ecxt, TLAM (a, e) ->
          (match infer (a::tcxt, ecxt, e) with
            Some t -> Some (TALL (a, t))
          | None -> None)
      | tcxt, ecxt, TAP (e, t) ->
          (match infer (tcxt, ecxt, e) with
            Some (TALL (a, t')) ->
              if type_check (tcxt, t)
                then Some (type_subst (t', a, t))
                else None
          | _ -> None) in
    fun e -> infer ([], [], e)

  let test () = begin
    let test_infer e t = begin
      print_endline (expr_to_string e);
      print_endline ("  " ^ typ_to_string t);
      match get_type e with
        Some t -> print_endline ("  " ^ typ_to_string t ^ "\n")
      | None -> print_endline "None\n"
    end in
    test_infer e0 t0;
    test_infer e3 t3
  end
end

module LC : sig
  type expr = VAR of string
            | LAM of string * expr
            | AP  of expr * expr
            | LET of string * expr * expr

  val expr_to_string : expr -> string
  val typeinfer : expr -> (STLC.expr * STLC.typ) option
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
      let rec occurs = function
          x, TVAR {contents = UNLINK x'} -> x = x'
        | x, TVAR {contents = LINK t }  -> occurs (x, t)
        | x, TARR (t1, t2) -> occurs (x, t1) || occurs (x, t2) in
      let rec unify = function
          TVAR {contents = LINK t}, t' | t', TVAR {contents = LINK t} -> unify (t, t')
        | TVAR ({contents = UNLINK x}), TVAR ({contents = UNLINK x'}) when x = x' ->
            true
        | TVAR ({contents = UNLINK x} as r), t | t, TVAR ({contents = UNLINK x} as r) ->
            if occurs (x, t) then false else (r := LINK t; true)
        | TARR (t1, t2), TARR (t1', t2') -> unify (t1, t1') && unify (t2, t2') in
      List.for_all unify (!equations) in

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
      let success = solve_equations () in
      if success
        then Some (expr' (), concretize_type t)
        else None
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

  (* \x. x x, not typeable *)
  let e3 = LAM ("x", AP (VAR "x", VAR "x"))

  let test () = begin
    let test_mono e = begin
      print_endline ("Inferring " ^ expr_to_string e);
      match typeinfer e with
          Some (e', t) -> print_endline ("  (" ^ STLC.expr_to_string e' ^ ") : " ^ STLC.typ_to_string t)
        | None -> print_endline "  not typeable"
    end in
    test_mono e0;
    test_mono e1;
    test_mono e2;
    test_mono e3
  end
end
