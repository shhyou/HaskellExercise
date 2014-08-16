val () = Control.Print.printDepth := 1000

datatype expr = VAR of string
              | LAM of string * expr
              | AP  of expr * expr
              | NUM of int
              | ADD of expr * expr

fun isValue (VAR x) = false
  | isValue (LAM (x,e)) = true
  | isValue (AP (e1,e2)) = false
  | isValue (NUM n) = true
  | isValue (ADD (e1,e2)) = false

(* Just a simple substitution, without handling capturing, etc. *)
(* We do not reduce under lambda, so I hopt it's fine... *)
fun subst x M (VAR y) = if x = y then M else (VAR y)
  | subst x M (LAM (y, N)) = LAM (y, if x = y then N else (subst x M N))
  | subst x M (AP (e1, e2)) = AP (subst x M e1, subst x M e2)
  | subst x M (NUM n) = NUM n
  | subst x M (ADD (e1, e2)) = ADD (subst x M e1, subst x M e2)

exception ReduceVar of string

fun reduce (VAR x) = raise (ReduceVar x)
  | reduce (LAM (x,e)) = LAM (x,e)
  | reduce (AP (LAM (x,e), e2)) =
      if isValue e2
      then (subst x e2 e)
      else AP (LAM (x,e), reduce e2)
  | reduce (AP (e1, e2)) = AP (reduce e1, e2)
  | reduce (NUM n) = NUM n
  | reduce (ADD (NUM n, NUM m)) = NUM (n+m)
  | reduce (ADD (NUM n, e2)) = ADD (NUM n, reduce e2)
  | reduce (ADD (e1, e2)) = ADD (reduce e1, e2)

fun reducestar prog 0 = [prog]
  | reducestar prog n = prog :: reducestar (reduce prog) (n-1)

(* (fn x => fn y => x + y) 5 8 *)
val prog1 =
  AP (AP (LAM ("x", LAM ("y", ADD (VAR "x", VAR "y"))),
          ADD (NUM 2, NUM 3)),
      NUM 8)
