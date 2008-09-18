structure TemplateCompiler :> TEMPLATE_COMPILER =
struct
  structure TG = TemplateGenerator

  (* val mix: 'a -> 'a list -> 'a list
   *
   * Intersperse the first parameter between each element in the second list:
   * mix 0 [ 1, 2, 3 ] ==> [ 1, 0, 2, 0, 3 ]
   *)
  fun mix _ nil = nil
    | mix _ (a::nil) = a :: nil
    | mix sep (a::rest) = a :: sep :: mix sep rest


  (* val compileGen: gen -> string
   *
   * Compile gen to ML code. All keys in TG.GenSubst nodes should exist in the
   * surrounding environment.
  *)
  fun compileGen (TG.GenText t) = "\"" ^ String.toString t ^ "\""
    | compileGen (TG.GenSubst (cvt, v)) = cvt ^ "(" ^ v ^ ")"
    | compileGen (TG.GenConcat gens) = String.concat [
          "String.concat[", 
          String.concat (mix "," (map compileGen gens)),
          "]" ] 
    | compileGen (TG.GenIterate (var, src, tree, sep)) = String.concat [
          case sep of "" => "String.concat"
                    | _ => "String.concatWith" ^ compileGen (TG.GenText sep),
          "(map(fn ", var, "=>",
          compileGen tree,
          ")(", src, "))" ]
    | compileGen (TG.GenCaseOf (var, terms)) = String.concat [
          "case ",
          var,
          " of",
          String.concat (mix "|" (map (fn (exp, gen) => String.concat [
                                                          "(", exp, ")=>(",
                                                          compileGen gen,
                                                          ")" ]
                                      ) terms)) ]


  (* val compile: string * string * gen -> string
   *
   * Compile gen to ML code. The first arugment is the name of the structure
   * to produce; the second is the type of arguments expected by gen.
   *)
  fun compile (sname, itype, gen) = String.concat [
          "structure ",
          sname,
          " = struct fun render ",
          itype,
          "=Web.HTML(",
          compileGen gen,
          ") end"
        ]

end
