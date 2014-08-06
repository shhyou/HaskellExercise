datatype value =
    zero
  | suc of value
  | lambda of (value -> value)

datatype expr =
    ZERO
  | SUC of expr
  | VAR of string
  | LAM of string * expr
  | AP  of expr * expr
  | IFZ of expr * expr * string * expr

(* N[M/x] is written subst x M N *)
(* this is only correct if there are *no* repeated names in N,M *)
fun subst x M ZERO               = ZERO
  | subst x M (SUC N)            = SUC (subst x M N)
  | subst x M (VAR y)            = if x = y then M else (VAR y)
  | subst x M (LAM (y, N))       =
      LAM (y, if x = y then N else (subst x M N))
  | subst x M (AP (e1, e2))      = AP (subst x M e1, subst x M e2)
  | subst x M (IFZ (e1, e2, y, e3)) =
      IFZ (subst x M e1, subst x M e2, y, if x = y then e3 else (subst x M e3))

fun eval gamma ZERO         = zero
  | eval gamma (SUC x)      = suc (eval gamma x)
  | eval gamma (VAR x)      = Env.lookup x gamma
  | eval gamma (LAM (x,e))  = lambda (fn v => eval (Env.extend x v gamma) e)
  | eval gamma (AP (e1,e2)) =
      let val (lambda f, v) = (eval gamma e1, eval gamma e2)
      in  f v end
  | eval gamma (IFZ (con,th,x,el)) =
      case eval gamma con
        of zero  => eval gamma th
         | suc n => eval (Env.extend x n gamma) el

(* (\n. SUC (SUC n)) (SUC ZERO) *)
val prog1 =
  AP ((LAM ("n", (SUC (SUC (VAR "n"))))),
      (SUC (SUC ZERO)))

(* pred := \n. ifz(n, 0, m. m) *)
val prog2 =
  LAM ("n",
       IFZ (VAR "n",
            ZERO,
            "m",
            VAR "m"))
