structure TinyXMLParser :> TINYXML_PARSER =
struct

  structure S = MStream
  structure T = Mlex.UserDeclarations
  structure X = TinyXML

  exception ParseError

  type tokenstream = T.lexresult S.stream

  (* val prepareLexer: string -> tokenstream

     Open the file of the specified name and return a (memoizing, lazy) stream of tokens.
  *)
  fun prepareLexer filename = let
    val inFile = TextIO.openIn filename
    val lexer =  Mlex.makeLexer (fn n => TextIO.inputN (inFile, n))
    fun delayLex () = MStream.Cons(lexer (), MStream.delay delayLex)
  in
    MStream.delay delayLex
  end

  fun parse_doc (ls: tokenstream) : (X.procinst list * X.element * tokenstream) = (
      let
        val (procinsts, ls') = parse_prologue ls
        val (docElement, ls') = case S.expose ls' of
                                    S.Cons(T.BEGINTAG tname, ls') => parse_tag ls' tname
                                  | _ => raise ParseError
      in
        (procinsts, docElement, ls')
      end
  )

  and parse_prologue (ls: tokenstream) : (X.procinst list * tokenstream) =
      case S.expose ls of
        S.Cons (T.DATA s, ls') => 
          if List.all Char.isSpace (String.explode s)
          then parse_prologue ls'
          else raise ParseError
      | S.Cons (T.BEGINTAG _, ls') => (nil, ls)
      | S.Cons (T.PI k, ls') => (
          case S.expose ls' of
            S.Cons (T.DATA v, ls') =>
              let
                val (rest, ls') = (parse_prologue ls')
              in
                ((X.XProcInst (k, v))::rest, ls')
              end
          | _ => raise ParseError
          )
      | _ => raise ParseError

  and parse_tag (ls: tokenstream) (tname: string) : (X.element * tokenstream) =
      let
        val (attlist, ls') = parse_attrs ls
      in
        (case S.expose ls' of
          S.Cons (T.TAGEND, ls') =>
            let
              val (children, ls') = parse_content ls'
            in
              case S.expose ls' of
                S.Cons(T.CLOSETAG tname', ls') => 
                  if (tname = tname')
                  then
                     (case S.expose ls' of
                        S.Cons(T.TAGEND, ls') => ((X.XElement (tname, attlist, children)), ls')
                      | _ => raise ParseError
                     )    
                else raise ParseError 
              | _ => raise ParseError
            end
        | S.Cons (T.TAGSELFCLOSE, ls') =>
            ((X.XElement (tname, attlist, [])), ls')

        | _ => raise ParseError
        )
      end

  and parse_content (ls: tokenstream) : (X.node list * tokenstream) = (
      case S.expose ls of
        S.Cons (T.DATA s, ls') =>
          let
            val (rest, ls'') = (parse_content ls')
          in
            ((X.XTextNode s)::rest, ls'')
          end
      | S.Cons (T.BEGINTAG tname, ls') => 
          let
            val (tag, ls') = parse_tag ls' tname
            val (rest, ls') = parse_content ls'
          in
            ((X.XElementNode tag)::rest, ls')
          end
      | S.Cons (T.CLOSETAG tname, ls') => (nil, ls)
      | _ => raise ParseError
  )

  and parse_attrs (ls: tokenstream) : (X.attribute list * tokenstream) = (
      case S.expose ls of
        S.Cons(T.ATTRIB attrname, ls') => (
          case S.expose ls' of
            S.Cons(T.ATTRIBVALUE attrvalue, ls'') =>
              let
                val (rest, ls''') = (parse_attrs ls'')
              in
                ((attrname, attrvalue)::rest, ls''')
              end
          | _ => (nil, ls')
        )
      | S.Cons(T.TAGEND, ls') => (nil, ls)
      | S.Cons(T.TAGSELFCLOSE, ls') => (nil, ls)
      | _ => raise ParseError
  )

  (* val parseFile: string -> TinyXML.node

     Load and parse the specified file.
  *)
  fun parseFile filename = let
    val (procinsts, docNode, leftoverStream) = parse_doc (prepareLexer filename)
    val remainder = S.expose leftoverStream
    val doc = X.XDocument (NONE, procinsts, docNode)
  in
    case remainder of
      S.Empty => doc
    | S.Cons (T.EOF, _) => doc
    | S.Cons (T.DATA s, more) => (
        if List.all Char.isSpace (String.explode s)
        then (case S.expose more of S.Empty => doc
                                  | S.Cons(T.EOF, _) => doc
                                  | _ => raise ParseError)
        else raise ParseError
      )
    | S.Cons (_, _) => raise ParseError
  end

end
