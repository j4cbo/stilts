signature TEMPLATE_ATTR =
sig

  type in_attr = TinyXML.attribute
  exception InvalidAttribute of in_attr

  type expr = string

  datatype attr = TAFor of expr * expr
                | TAIf of expr
                | TAIfOption of expr * expr
                | TACase of expr
                | TAOf of expr

  val process: in_attr list -> (attr list * in_attr list) 

  val separateOf: in_attr list -> (expr * in_attr list) option

end
