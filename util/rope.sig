signature ROPE = sig

  eqtype rope
  val fromString: string -> rope
  val fromStrings: string list -> rope
  val fromRopes: rope list -> rope

  val toString: rope -> string

end
