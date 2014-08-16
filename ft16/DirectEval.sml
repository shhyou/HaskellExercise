datatype value = FN of (value -> value)
               | N  of int

datatype expr = VAR of string
              | LAM of string * expr
              | AP  of expr * expr
              | NUM of int
              | ADD of expr * expr

fun eval gamma (VAR x) = Env.lookup x gamma
  | eval gamma (LAM (x, e)) = FN (fn v => eval (Env.extend x v gamma) e)
  | eval gamma (AP (e1, e2)) =
      let val (FN f, v) = (eval gamma e1, eval gamma e2)
      in  f v  end
  | eval gamma (NUM n) = N n
  | eval gamma (ADD (e1, e2)) =
      let val (N n, N m) = (eval gamma e1, eval gamma e2)
      in  N (n+m) end

(* (fn x => fn y => x + y) 5 8 *)
val prog1 =
  AP (AP (LAM ("x", LAM ("y", ADD (VAR "x", VAR "y"))),
          ADD (NUM 2, NUM 3)),
      NUM 8)
