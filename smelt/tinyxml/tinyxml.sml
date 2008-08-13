structure TinyXML =
struct
  type nodename = string
  type attribute = string * string

  type dtd = string

  datatype document = XDocument of dtd option * procinst list * element
  and element = XElement of nodename * attribute list * node list
  and procinst = XProcInst of string * string
  and node = XElementNode of element
           | XTextNode of string
           | XPINode of procinst

  fun escape t = String.translate (fn #"<" => "&lt;"
                                    | #"&" => "&amp;"
                                    | #"\"" => "&quot;"
                                    | c => String.str c) t

  fun serialize (XDocument (_, _, root)) =
    let
      fun serializeAttr (key, value) = Rope.fromStrings([ " ", key, "=\"", (escape value), "\"" ])

      fun serializeNode (XTextNode t) = Rope.fromString (escape t)
        | serializeNode (XElementNode (XElement (tag, attrs, nodes))) =
            Rope.fromRopes [
                              Rope.fromString ("<" ^ tag),
                              Rope.fromRopes (map serializeAttr attrs),
                              Rope.fromString ">",
                              Rope.fromRopes (map serializeNode nodes),
                              Rope.fromString ("</" ^ tag ^ ">")
                           ] 
        | serializeNode (XPINode (XProcInst (k, v))) =
            Rope.fromString ("<?" ^ (escape k) ^ (escape v) ^ "?>")
    in
      Rope.toString (serializeNode (XElementNode root))
    end

end
