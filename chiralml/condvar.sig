signature CONDVAR = sig

  type t

  val new: unit -> t

  val wait: t -> unit
  val signal: t -> bool
  val broadcast: t -> int

end
