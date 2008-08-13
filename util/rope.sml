structure Rope :> ROPE =
struct

  datatype rope = RString of string
                | RList of rope list

  val fromString = RString
  fun fromStrings sl = RList(map RString sl)
  val fromRopes = RList

  fun toString (RString(s)) = s
    | toString (RList(rl)) = String.concat (map toString rl) 

end
