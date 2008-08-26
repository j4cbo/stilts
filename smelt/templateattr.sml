structure TemplateAttr :> TEMPLATE_ATTR = struct

  (* Attribute parsing. *)

  type in_attr = TinyXML.attribute
  exception InvalidAttribute of in_attr

  type expr = string

  datatype attr = TAFor of expr * expr
                | TAIf of expr
                | TAIfOption of expr * expr
                | TAStrip of expr
                | TACase of expr
                | TAOf of expr


  (* val isBefore: (attr * attr) -> bool
   *
   * Provide an ordering on template attributes.
   *)
  fun isBefore (a, b) = let
      fun ordering (TAOf _) = 6
        | ordering (TAFor _) = 5
        | ordering (TAIfOption _) = 4
        | ordering (TAIf _) = 3
        | ordering (TAStrip _) = 2
        | ordering (TACase _) = 1
    in
      ordering a < ordering b
    end


  (* val fromKV: in_attr -> attr
   *
   * Convert an XML attribute to a template attribute (type attr) value.
   *)
  fun fromKV (k as "t:for", v) = let
        val (fst, rest) = Substring.position " in " (Substring.full v)
      in
        if Substring.isPrefix " in " rest
        then TAFor (Substring.string fst,
                    Substring.string (Substring.triml 4 rest))
        else raise InvalidAttribute (k, v)
      end
    | fromKV ("t:if", v) = TAIf v
    | fromKV ("t:ifOption", v) = let
        val (fst, rest) = Substring.position " as " (Substring.full v)
      in
        if Substring.isPrefix " as " rest
        then TAIfOption (Substring.string fst,
                         Substring.string (Substring.triml 4 rest))
        else TAIfOption (v, v)
      end
    | fromKV ("t:case", v) = TACase v
    | fromKV ("t:of", v) = TAOf v
    | fromKV ("t:strip", v) = TAStrip v
    | fromKV (k, v) = raise InvalidAttribute (k, v)


  (* val orderedFromKV: in_attr list -> attr list
   *
   * Convert an (arbitrarily-ordered) list of XML attributes to an ordered
   * list of template attributes.
  *)
  fun orderedFromKV list = ListMergeSort.sort isBefore (map fromKV list)


  (* val process: in_attr list -> (attr list * in_attr list)
   *
   * Process a list of XML attributes, separating the template-language ones
   * from others, and then parse the template-language attributes.

   * If an unrecognized "t:" attribute is encoutered, this will raise an
   * InvalidAttribute exception with the attribute in question.
   *)
  fun process' accT accO nil = (orderedFromKV accT, accO)
    | process' accT accO ((k, v)::rest) = if String.isPrefix "t:" k
                                        then process' ((k, v)::accT) accO rest
                                        else process' accT ((k, v)::accO) rest
  val process = process' nil nil


  (* val separateOf: in_attr list -> (expr * in_attr list)
   *
   * Scan through the input list searching for a t:of attribute. If found,
   * return SOME (t:of value, other attributes); otherwise return NONE.
   *)
  fun separateOf' acc nil = NONE
    | separateOf' acc (("t:of", v:string)::rest) = SOME (v, acc @ rest)
    | separateOf' acc ((k, v)::rest) = separateOf' ((k, v)::acc) rest
  val separateOf = separateOf' nil

end
