signature TEMPLATE_GENERATOR =
sig
  exception ParseError of string

  type expr = string
  datatype gen = GenText of string
               | GenSubst of string * expr
               | GenConcat of gen list
               | GenIterate of expr * expr * gen * string
               | GenCaseOf of expr * (expr * gen) list

  val generate: TinyXML.document -> string * string * gen 


  val optimizeGen: gen -> gen
end
