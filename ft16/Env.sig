signature ENV = sig
  type 'a env
  val empty : 'a env
  val extend : string -> 'a -> 'a env -> 'a env
  exception Unbound of string
  val lookup : string -> 'a env -> 'a
end
