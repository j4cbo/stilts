structure TemplateGenerator :> TEMPLATE_GENERATOR =
struct
  exception ParseError of string

  structure X = TinyXML
  structure SS = Substring

  type expr = string
  datatype gen = GenText of string
               | GenSubst of string * expr
               | GenConcat of gen list
               | GenIterate of expr * expr * gen
               | GenCaseOf of expr * (expr * gen) list

  structure TA = TemplateAttr

  (* val genTextExpression: string -> gen
   *
   * Parse out all ${expressions} in the input, and produce a gen that will
   * substitute and render all values into an XML-safe string.
   *)
  fun genTextExpression s =
    let
      fun r acc = String.implode (rev acc)
      fun escaped acc = GenText (X.escape (r acc))

      fun parseText nil nil = nil
        | parseText acc nil = (escaped acc) :: nil
        | parseText acc (#"$"::(#"{"::rest)) =
            (escaped acc) :: (parseSubst nil rest)
        | parseText acc (#"$"::(#"H"::(#"{"::rest))) =
            (escaped acc) :: (parseRawSubst nil rest)
        | parseText acc (ch::rest) = parseText (ch::acc) rest
      and parseSubst acc nil = raise ParseError "missing } in substitution"
        | parseSubst acc (#"}"::rest) =
            GenSubst("WebUtil.escapeStr", r acc)::(parseText nil rest)
        | parseSubst acc (ch::rest) = parseSubst (ch::acc) rest
      and parseRawSubst acc nil = raise ParseError "missing } in substitution"
        | parseRawSubst acc (#"}"::rest) =
            GenSubst("(fn Web.HTML s => s)", r acc)::(parseText nil rest)
        | parseRawSubst acc (ch::rest) = parseRawSubst (ch::acc) rest
    in
      GenConcat(parseText nil (String.explode s))
    end


  (* val genAttr: TinyXML.attribute -> gen
   *
   * Generate a key="value" string for this attribute, interpolating variables
   * with genTextExpression.
   *)
  fun genAttr (k, v) =
    GenConcat([
      GenText(" " ^ k ^ "=\""),
      (genTextExpression v),
      GenText("\"")
    ])


  (* val genElement: string * TinyXML.attribute list * gen option -> gen
   *
   * Generate the specified XML element. The gen option is SOME g if the
   * element has children, NONE if it does not.
   *)
  fun genElement (name, attrs, NONE) =
        GenConcat [
          GenText ("<" ^ name),
          GenConcat (map genAttr attrs),
          GenText " />"
        ]
    | genElement (name, attrs, SOME children) =
        GenConcat [
          GenText ("<" ^ name),
          GenConcat (map genAttr attrs),
          GenText ">", 
          children,
          GenText ("</" ^ name ^ ">")
        ]


  (* val genOfNode: TinyXML.node -> (expr * gen) option
   *
   * Confirm that each node contains a t:of option, and then return a list of
   * (expr, gen) tuples suitable for GenCaseOf.
   *)
  fun genOfNode (X.XTextNode s) =
        if List.all Char.isSpace (String.explode s) then NONE
        else raise ParseError "non-whitespace text between t:case and t:of"
    | genOfNode (X.XElementNode (X.XElement (name, attrs, children))) = (
        case TA.separateOf attrs of
          NONE => raise ParseError "node without t:of inside t:case"
        | SOME (exp, others) => SOME
           (exp, genNode (X.XElementNode (X.XElement (name, others, children))))
      )
    | genOfNode (X.XPINode (X.XProcInst (k, v))) =
        raise ParseError ("unexpected processing instruction: " ^ k ^ " " ^ v) 


  (* val genNode: TinyXML.node -> gen
   *
   * Recursively generate the specified XML node.
   *)
  and genNode (X.XTextNode s) = genTextExpression s
    | genNode (X.XElementNode (X.XElement (name, attrs, children))) =
      let
        val (templateAttrs, commonAttrs) = TemplateAttr.process attrs
        
        fun attrLoop nil =
              genElement (name, commonAttrs,
                          case children of
                             nil => NONE
                           | c => SOME (GenConcat (map genNode children)))
          | attrLoop ((TA.TAFor (bindVar, iterVar)) :: rest) =
              GenIterate(bindVar, iterVar, attrLoop rest)
          | attrLoop ((TA.TAIf exp) :: rest) =
              GenCaseOf (exp, [ ("true", attrLoop rest),
                                ("false", GenText "") ])
          | attrLoop ((TA.TAIfOption (exp, binding)) :: rest) =
              GenCaseOf (exp, [ (("SOME(" ^ binding ^ ")"), attrLoop rest),
                                ("NONE", GenConcat nil) ])
          | attrLoop ((TA.TACase exp) :: nil) =
              genElement (name, commonAttrs,
                          SOME (GenCaseOf (exp,
                                           List.mapPartial genOfNode children)))
          | attrLoop ((TA.TACase exp) :: _) =
              raise Fail "Smelt internal: TACase not last in sorted list?"
          | attrLoop ((TA.TAOf exp) :: _) =
              raise ParseError "t:of outside t:case"
      in
        attrLoop templateAttrs
      end
    | genNode (X.XPINode (X.XProcInst (k, v))) =
        raise ParseError ("unexpected processing instruction: " ^ k ^ " " ^ v) 


  (* val optimizeGen: gen -> gen

     Optimize (by flattening adjacent GenText nodes, redundant GenConcat nodes,
     etc.) the generator tree.
  *) 
  fun optimizeGen (GenConcat (g::nil)) = g
    | optimizeGen (GenConcat l) =
    let
      fun optConcat nil = nil 
        | optConcat ((GenText s1)::((GenText s2)::r)) =
            optConcat ((GenText (s1^s2))::r)
        | optConcat ((GenConcat nil)::r) = optConcat r 
        | optConcat ((GenConcat inner)::r) = optConcat (inner @ r)
        | optConcat (v::((GenConcat nil)::r)) = optConcat (v::r)
        | optConcat (v::((GenConcat inner)::r)) = optConcat (v::(inner @ r))
        | optConcat (gen::rest) = (optimizeGen gen)::(optConcat rest)
    in
      GenConcat(optConcat l)
    end
    | optimizeGen (GenIterate (k, v, t)) = GenIterate (k, v, optimizeGen t)
    | optimizeGen g = g


  (* val findTemplatePI: X.procinst list -> string option

     Return the value of the first <?template ... ?> processing instruction,
     if any.
  *)
  fun findTemplatePI nil = NONE
    | findTemplatePI ((X.XProcInst (k, v))::rest) =
        case k of "template" => SOME v
                | _ => findTemplatePI rest 


  (* val generate: X.document -> string * string * gen
   *
   * Main function. Retunrs a tuple of (structure name, input type, generator).
   *)
  fun generate (X.XDocument (_, procinsts, e as X.XElement _)) =
        case findTemplatePI procinsts of
          NONE => raise ParseError "missing <?template ... ?> declaration"
        | SOME s => let
              val (sname, itype) = SS.splitl (not o Char.isSpace) (SS.full s)
            in
              (SS.string sname, SS.string itype,
               optimizeGen (genNode (X.XElementNode e)))
            end
end
