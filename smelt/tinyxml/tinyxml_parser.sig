signature TINYXML_PARSER =
sig

  exception ParseError

  val parseFile: string -> TinyXML.document

end
