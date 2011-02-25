signature FORM = sig

  structure Map : ORD_MAP where type Key.ord_key = string
  type form = string list Map.map

  val load: Web.request -> form

  val get: form -> string -> string option
  val getAll: form -> string -> string list

  val export: form -> string
  val import: string -> form

  val dump: form -> string

end
