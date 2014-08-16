val () = Control.Print.printDepth := 1000

datatype expr = VAR of string
              | LAM of string * expr
              | AP  of expr * expr
              | NUM of int
              | ADD of expr * expr

(* Just a simple substitution, without handling capturing, etc. *)
fun subst x M (VAR y) = if x = y then M else (VAR y)
  | subst x M (LAM (y, N)) = LAM (y, if x = y then N else (subst x M N))
  | subst x M (AP (e1, e2)) = AP (subst x M e1, subst x M e2)
  | subst x M (NUM n) = NUM n
  | subst x M (ADD (e1, e2)) = ADD (subst x M e1, subst x M e2)

exception ReduceVar of string

fun step (VAR x) = raise (ReduceVar x)
  | step (LAM (x, e)) = LAM (x, e)
  | step (AP (e1, e2)) =
      let
        val LAM (x, e) = step e1
        val v = step e2
      in
        step (subst x v e)
      end
  | step (NUM n) = NUM n
  | step (ADD (e1, e2)) =
      let
        val (NUM n1) = step e1
        val (NUM n2) = step e2
      in
        NUM (n1 + n2)
      end

(* (fn x => fn y => x + y) 5 8 *)
val prog1 =
  AP (AP (LAM ("x", LAM ("y", ADD (VAR "x", VAR "y"))),
          ADD (NUM 2, NUM 3)),
      NUM 8)
