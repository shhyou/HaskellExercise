exception Undefined;;

let paren b s = if b then "(" ^ s ^ ")" else s

(* remove duplicated terms, O(n^2) *)
let nub =
  let rec remove = function
      cxt, [] -> []
    | cxt, x::xs -> if List.mem x cxt then remove (cxt, xs) else x::remove (x::cxt, xs) in
  fun xs -> remove ([], xs)

(* xs \\ ys *)
let diff xs ys = List.filter (fun x -> not (List.mem x ys)) xs

module STLC : sig
  type expr = VAR of string
            | LAM of string * typ * expr
            | AP  of expr * expr
            | LET of string * expr * expr

  and typ = TVAR of string
          | TARR of typ * typ

  exception Type_mismatch of (string * typ) list * expr * typ * typ
  val expr_to_string : expr -> string
  val typ_to_string : typ -> string
  val get_type : expr -> typ
  val test : unit -> unit
end =
struct
  type expr = VAR of string
            | LAM of string * typ * expr
            | AP  of expr * expr
            | LET of string * expr * expr

  and typ = TVAR of string
          | TARR of typ * typ

  exception Type_mismatch of (string * typ) list * expr * typ * typ

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
        cxt, VAR x -> List.assoc x cxt
      | cxt, LAM (x, t1, e) -> TARR (t1, infer ((x,t1)::cxt, e))
      | cxt, (AP (e1, e2) as e) ->
         (match infer (cxt, e1), infer (cxt, e2) with
           TARR (t1, t2), t1' when t1 = t1' -> t2
         | t, t1' -> raise (Type_mismatch (cxt, e, t, TARR (t1', TVAR "_?"))))
      | cxt, LET (x, e1, e2) -> infer ((x,infer (cxt, e1))::cxt, e2) in
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
      print_endline ("  " ^ typ_to_string (get_type e) ^ "\n")
    end in
    begin
      test_check (e0, t0);
      test_check (e1, t1);
      test_check (e2, t2)
    end
end

module SysF : sig
  type expr = VAR  of string
            | LAM  of string * typ * expr
            | AP   of expr * expr
            | LET  of string * expr * expr
            | TLAM of string * expr
            | TAP  of expr * typ

  and typ = TVAR of string
          | TARR of typ * typ
          | TALL of string * typ

  exception Type_mismatch of (string * typ) list * expr * typ * typ
  exception Malformed_type of string list * typ
  val expr_to_string : expr -> string
  val typ_to_string : typ -> string
  val type_check : string list * typ -> bool
  val type_equal : typ * typ -> bool
  val type_subst : typ * string * typ -> typ
  val get_type : expr -> typ
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

  exception Type_mismatch of (string * typ) list * expr * typ * typ
  exception Malformed_type of string list * typ

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
    let rec index = function
        _, _, [] -> raise Not_found
      | n, x, y::ys -> if x = y then n else index (n+1, x, ys) in
    let rec check = function
        cxt1, cxt2, TVAR x, TVAR y ->
          (match List.mem x cxt1, List.mem y cxt2 with
            true, true -> index (0, x, cxt1) = index (0, y, cxt2)
          | false, false -> x = y
          | _ -> false)
      | cxt1, cxt2, TARR (t1, t2), TARR (t1', t2') ->
          check (cxt1, cxt2, t1, t1') && check (cxt1, cxt2, t2, t2')
      | cxt1, cxt2, TALL (a1, t1), TALL (a2, t2) ->
          check (a1::cxt1, a2::cxt2, t1, t2)
      | _ -> false in
    fun (t1, t2) -> check ([], [], t1, t2)

  (* type_subst (t, a, t') := t [t' / a], substitute t' for a in t *)
  let type_subst =
    let fresh_sym =
      let cnt = ref 0 in
      fun () -> begin
        cnt := !cnt + 1;
        "T" ^ string_of_int (!cnt)
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
          TALL (y, subst (alpha_conv (t, y, x), a, t')) in
    subst

  let get_type =
    let rec infer = function
        tcxt, ecxt, VAR x -> List.assoc x ecxt
      | tcxt, ecxt, LAM (x, t, e) ->
          if type_check (tcxt, t)
            then TARR (t, infer (tcxt, (x,t)::ecxt, e))
            else raise (Malformed_type (tcxt, t))
      | tcxt, ecxt, (AP (e1, e2) as e) ->
          (match infer (tcxt, ecxt, e1), infer (tcxt, ecxt, e2) with
            TARR (t1, t2), t1' when type_equal (t1, t1') -> t2
          | t, t1' -> raise (Type_mismatch (ecxt, e, t, TARR (t1', TVAR "_?"))))
      | tcxt, ecxt, LET (x, e1, e2) -> infer (tcxt, (x,infer (tcxt, ecxt, e1))::ecxt, e2)
      | tcxt, ecxt, TLAM (a, e) -> TALL (a, infer (a::tcxt, ecxt, e))
      | tcxt, ecxt, (TAP (e, t) as e') ->
          (match type_check (tcxt, t), infer (tcxt, ecxt, e) with
            false, _ -> raise (Malformed_type (tcxt, t))
          | true, TALL (a, t') -> type_subst (t', a, t)
          | _, t -> raise (Type_mismatch (ecxt, e', t, TALL ("_?", TVAR "_?")))) in
    fun e -> infer ([], [], e)

  let test () = begin
    let test_infer e t = begin
      print_endline (expr_to_string e);
      print_endline ("  " ^ typ_to_string t);
      print_endline ("  " ^ typ_to_string (get_type e) ^ "\n")
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

  exception STLC_no_unify of expr * STLC.typ * STLC.typ
  exception STLC_infinite_type of expr * STLC.typ * STLC.typ
  val typeinfer : expr -> STLC.expr * STLC.typ

  exception SysF_no_unify of expr * SysF.typ * SysF.typ
  exception SysF_infinite_type of expr * SysF.typ * SysF.typ
  val typeinfer_hm : expr -> SysF.expr * SysF.typ

  val test : unit -> unit
end =
struct
  let fresh_sym =
    let cnt = ref 0 in
    fun prefix -> begin
      cnt := !cnt + 1;
      prefix ^ string_of_int (!cnt)
    end

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

  let rec to_STLC_typ = function
      TVAR {contents = UNLINK x} -> STLC.TVAR x
    | TVAR {contents = LINK t} -> to_STLC_typ t
    | TARR (t1, t2) -> STLC.TARR (to_STLC_typ t1, to_STLC_typ t2)

  let rec to_SysF_typ = function
      TVAR {contents = UNLINK x} -> SysF.TVAR x
    | TVAR {contents = LINK t} -> to_SysF_typ t
    | TARR (t1, t2) -> SysF.TARR (to_SysF_typ t1, to_SysF_typ t2)

  let type_fvs =
    let rec fvs = function
        cxt, TVAR {contents = UNLINK x} -> if not (List.mem x cxt) then [x] else []
      | cxt, TVAR {contents = LINK t} -> fvs (cxt, t)
      | cxt, TARR (t1, t2) -> fvs (cxt, t1) @ fvs (cxt, t2) in
    fun t -> nub (fvs ([], t))

  type unify_error = SHAPE_MISMATCH | OCCURS_CHECK
  exception UnifyError of unify_error

  exception STLC_no_unify of expr * STLC.typ * STLC.typ
  exception STLC_infinite_type of expr * STLC.typ * STLC.typ

  let fresh_var () = TVAR (ref (UNLINK (fresh_sym "T")))

  let typeinfer expr =
    let equations : (expr * typ * typ) list ref = ref [] in

    let add_equation eq = equations := eq::(!equations) in

    let print_equations () =
      List.iter
        (fun (e, t1, t2) ->
          print_endline (STLC.typ_to_string (to_STLC_typ t1) ^ " = "
                       ^ STLC.typ_to_string (to_STLC_typ t2)))
        (!equations) in

    let solve_equations () =
      let rec occurs = function
          x, TVAR {contents = UNLINK x'} -> x = x'
        | x, TVAR {contents = LINK t }  -> occurs (x, t)
        | x, TARR (t1, t2) -> occurs (x, t1) || occurs (x, t2) in

      let rec unify = function
          TVAR {contents = LINK t}, t' | t', TVAR {contents = LINK t} -> unify (t, t')
        | TVAR ({contents = UNLINK x}), TVAR ({contents = UNLINK x'}) when x = x' -> ()
        | TVAR ({contents = UNLINK x} as r), t | t, TVAR ({contents = UNLINK x} as r) ->
            if occurs (x, t) then raise (UnifyError OCCURS_CHECK) else r := LINK t
        | TARR (t1, t2), TARR (t1', t2') -> begin
            unify (t1, t1');
            unify (t2, t2')
        end in

      List.iter (fun (e, t1, t2) ->
                  try unify (t1, t2) with
                    UnifyError SHAPE_MISMATCH -> raise (STLC_no_unify (e, to_STLC_typ t1, to_STLC_typ t2))
                  | UnifyError OCCURS_CHECK -> raise (STLC_infinite_type (e, to_STLC_typ t1, to_STLC_typ t2)))
                (!equations) in

    let rec gen_cons = function
        cxt, VAR x -> (fun () -> STLC.VAR x), List.assoc x cxt
      | cxt, LAM (x, e) ->
          let t = fresh_var () in
          let e', t' = gen_cons ((x, t)::cxt, e) in
          (fun () -> STLC.LAM (x, to_STLC_typ t, e' ())), TARR (t, t')
      | cxt, (AP (e1, e2) as e) ->
          let e1', t1 = gen_cons (cxt, e1) in
          let e2', t2 = gen_cons (cxt, e2) in
          let t = fresh_var () in begin
            add_equation (e, t1, TARR (t2, t));
            (fun () -> STLC.AP (e1' (), e2' ())), t
          end
      | cxt, LET (x, e1, e2) ->
          let e1', t = gen_cons (cxt, e1) in
          let e2', t' = gen_cons ((x, t)::cxt, e2) in
          (fun () -> STLC.LET (x, e1' (), e2' ())), t' in

    let expr', t = gen_cons ([], expr) in
    begin
      print_equations ();
      solve_equations ();
      expr' (), to_STLC_typ t
    end

  exception SysF_no_unify of expr * SysF.typ * SysF.typ
  exception SysF_infinite_type of expr * SysF.typ * SysF.typ

  type typescheme = POLY of string list * typ

  let typeinfer_hm expr =
    let fresh_sym =
      let cnt = ref 0 in
      fun () -> begin
        cnt := !cnt + 1;
        "t" ^ string_of_int (!cnt)
      end in

    let instantiate cxt =
      let rec inst = function
          TVAR {contents = LINK t} -> inst t
        | TVAR {contents = UNLINK x} as t -> (try List.assoc x cxt with Not_found -> t)
        | TARR (t1, t2) -> TARR (inst t1, inst t2) in
      inst in

    let rec occurs = function
        x, TVAR {contents = UNLINK x'} -> x = x'
      | x, TVAR {contents = LINK t}  -> occurs (x, t)
      | x, TARR (t1, t2) -> occurs (x, t1) || occurs (x, t2) in

    let rec unify = function
        TVAR {contents = LINK t}, t' | t', TVAR {contents = LINK t} -> unify (t, t')
      | TVAR ({contents = UNLINK x}), TVAR ({contents = UNLINK x'}) when x = x' -> ()
      | TVAR ({contents = UNLINK x} as r), t | t, TVAR ({contents = UNLINK x} as r) ->
          if occurs (x, t) then raise (UnifyError OCCURS_CHECK) else r := LINK t
      | TARR (t1, t2), TARR (t1', t2') -> begin
          unify (t1, t1');
          unify (t2, t2')
      end in

    let quantify (cxt, e1', t) =
      let fvs = List.concat (List.map (fun (_, POLY (qvars, t)) -> diff (type_fvs t) qvars) cxt) in
      let qvars = diff (type_fvs t) fvs in
      let make_tlam e qvar = SysF.TLAM (qvar, e) in
      (fun () -> List.fold_left make_tlam (e1' ()) qvars), POLY (qvars, t) in

    let rec infer = function
        cxt, VAR x ->
          let POLY (qvars, t) = List.assoc x cxt in
          let vars = List.map (fun qvar -> qvar, fresh_var ()) qvars in
          let make_tap e (_, var) = SysF.TAP (e, to_SysF_typ var) in
          (fun () -> List.fold_left make_tap (SysF.VAR x) vars), instantiate vars t
      | cxt, LAM (x, e) ->
          let t = fresh_var () in
          let e', t' = infer ((x, POLY ([], t))::cxt, e) in
          (fun () -> SysF.LAM (x, to_SysF_typ t, e' ())), TARR (t, t')
      | cxt, (AP (e1, e2) as e) ->
          let e1', t1 = infer (cxt, e1) in
          let e2', t2 = infer (cxt, e2) in
          let t = fresh_var () in begin
            (try unify (t1, TARR (t2, t)) with
              UnifyError SHAPE_MISMATCH -> raise (SysF_no_unify (e, to_SysF_typ t1, to_SysF_typ t2))
            | UnifyError OCCURS_CHECK -> raise (SysF_infinite_type (e, to_SysF_typ t1, to_SysF_typ t2)));
            (fun () -> SysF.AP (e1' (), e2' ())), t
          end
      | cxt, LET (x, e1, e2) ->
          let e1', t = infer (cxt, e1) in
          let e1'', t'' = quantify (cxt, e1', t) in
          let e2', t' = infer ((x, t'')::cxt, e2) in
          (fun () -> SysF.LET (x, e1'' (), e2' ())), t' in

    let expr, t = infer ([], expr) in
    let expr', POLY (qvars', t') = quantify ([], expr, t) in
    expr' (), List.fold_left (fun e qvar -> SysF.TALL (qvar, e)) (to_SysF_typ t') qvars'

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

  let e4 = LET ("id", LAM ("x", VAR "x"), AP (VAR "id", VAR "id"))

  let e5 = LAM ("x", LAM ("f", AP (VAR "f", VAR "x")))

  let test () = begin
    let test_mono e = begin
      print_endline ("Inferring " ^ expr_to_string e);
      try
        let (e', t) = typeinfer e in
        print_endline ("  (" ^ STLC.expr_to_string e' ^ ") : " ^ STLC.typ_to_string t)
      with
        STLC_infinite_type _
      | STLC_no_unify _ -> print_endline "  not typeable"
    end in
    print_endline "==== Testing simply-typed lambda calculus ====";
    List.iter test_mono [e0; e1; e2; e3; e4; e5];
    let test_poly e = begin
      print_endline ("Inferring " ^ expr_to_string e);
      try
        let (e', t) = typeinfer_hm e in
        print_endline ("  (" ^ SysF.expr_to_string e' ^ ") : " ^ SysF.typ_to_string t)
      with
        SysF_infinite_type _
      | SysF_no_unify _ -> print_endline "  not typeable"
    end in
    print_endline "==== Testing polymorphic lambda calculus ====";
    List.iter test_poly [e0; e1; e2; e3; e4; e5];
  end
end
