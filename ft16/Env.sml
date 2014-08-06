structure Env :> ENV = struct
  type 'a env = (string * 'a) list

  val empty = []

  fun extend x v env = (x,v) :: env

  exception Unbound of string

  fun lookup x env =
    case List.find (fn (y,_) => x = y) env
      of (SOME (_,v)) => v
       | NONE         => raise (Unbound x)
end
