structure DirectEval = struct

datatype value =
    ZERO
  | SUC of value
  | FN of (value -> value)

datatype lam =
    VAR of string
  | LAM of string * lam
  | AP  of lam * lam

fun eval gamma (VAR x)      = Env.lookup x gamma
  | eval gamma (LAM (x,e))  = FN (fn v => eval (Env.extend x v gamma) e)
  | eval gamma (AP (e1,e2)) =
    let val (FN f, v) = (eval gamma e1, eval gamma e2)
    in (f v) end

fun main (prog_name, args) = (print "Hello, world!\n"; OS.Process.success)

end