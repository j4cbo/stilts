(* JSON parser and formatter.
 *
 * Portions of this are based on the MLton basis library;
 * see http://mlton.org/License
 *)

structure JSON = struct

  structure Map = BinaryMapFn (type ord_key = string
                               val compare = String.compare)

  datatype json = String of string
                | Number of IntInf.int
                | Real of Real.real
                | Object of json Map.map
                | Array of json list
                | Bool of bool
                | Null

  fun fmt (String s) = let
                         fun esc c = "\\u" ^ StringCvt.padLeft #"0" 4
                                               (Int.toString (Char.ord c))
                         fun trans #"\"" = "\\\""
                           | trans #"\\" = "\\\\"
                           | trans c = if Char.isPrint c then String.str c
                                                         else esc c
                       in
                         "\"" ^ String.translate trans s ^ "\""
                       end
    | fmt (Number i) = IntInf.toString i
    | fmt (Real r) = Real.toString r
    | fmt (Object m) = let
                         fun trans (k, v) = fmt (String k) ^ ":" ^ fmt v
                         val items = map trans (Map.listItemsi m)
                       in
                         "{" ^ String.concatWith "," items ^ "}"
                       end
    | fmt (Array js) = "[" ^ String.concatWith "," (map fmt js) ^ "]"
    | fmt (Bool true) = "true"
    | fmt (Bool false) = "false"
    | fmt (Null) = "null"


  structure PC = ParserComb

  fun scan r = let

        (* Combinator-based JSON parser. *)

        fun hex4 reader state = let
              fun loop (0, acc, state) = SOME (acc, state)
                 | loop (n, acc, state) = case reader state of
                            NONE => NONE
                          | SOME (c, state) =>
                              case Int.fromString (String.str c) of
                                NONE => NONE
                              | SOME n => loop (n - 1, n + acc * 16, state)
            in
              loop (4, 0, state)
            end


        fun scanChar reader state = let
              fun escape state = case reader state of
                    NONE => NONE
                  | SOME (c, state') => let
                        fun yes c = SOME (c, state')
                      in
                         case c of
                             #"\"" => yes #"\""   | #"\\" => yes #"\\"
                           | #"/" => yes #"/"     | #"b" => yes #"\b"
                           | #"f" => yes #"\f"    | #"n" => yes #"\n"
                           | #"r" => yes #"\r"    | #"t" => yes #"\t"
                           | #"u" => (
                              case hex4 reader state' of
                                  NONE => NONE
                                | SOME (d, s') => (SOME (Char.chr d, s')
                                                   handle Chr => NONE))
                           | _ => NONE
                      end
            in
              case reader state of
                 NONE => NONE
               | SOME (c, state) => case c of #"\\" => escape state
                                            | #"\"" => NONE
                                            | _ => SOME (c, state)
            end


        fun scanString reader state = let
              fun loop (state, cs) =
                    case scanChar reader state of
                      NONE => SOME (implode (rev cs), state)
                    | SOME (c, state) => loop (state, c :: cs)
            in
              loop (state, [])
            end

        fun seqWith3 f (a, b, c) = PC.seqWith
                                   (fn (x, (y, z)) => f (x, y, z))
                                   (a, PC.seq (b, c))

        fun whitespace r = PC.zeroOrMore (PC.eatChar Char.isSpace) r

        fun map_together (first, rest) =
              Map.insert' (first, foldl Map.insert' Map.empty rest)

        fun scanQString r =
              seqWith3 #2 (PC.char #"\"", scanString, PC.char #"\"") r
 
        fun scanValue r =
              seqWith3 #2 (
                whitespace,
                PC.or' [
                  PC.wrap (scanQString, String),
                  PC.wrap (scanArr, Array),
                  PC.wrap (scanObj, Object),
                  PC.wrap (PC.string "null", fn _ => Null),
                  PC.wrap (PC.string "true", fn _ => Bool true),
                  PC.wrap (PC.string "false", fn _ => Bool false),
                  PC.wrap (IntInf.scan StringCvt.DEC, Number),
                  PC.wrap (Real.scan, Real)
                ],
                whitespace
              ) r

        and scanArr r =
              PC.seqWith #2 (
                PC.char #"[",
                PC.or (
                  PC.wrap (PC.seq (whitespace, PC.char #"]"), fn _ => nil),
                  PC.seqWith (op ::) (
                    scanValue,
                    PC.seqWith #1 (
                      PC.zeroOrMore (PC.seqWith #2 (PC.char #",", scanValue)),
                      PC.eatChar (fn c => (c = #"]"))
                    )
                  )
                )
              ) r

        and scanKeyValue r =
              PC.seq (
                seqWith3 #2 (whitespace, scanQString, whitespace),
                PC.seqWith #2 (PC.char #":", scanValue)
              ) r

        and scanObj r =
              PC.seqWith #2 (
                PC.char #"{",
                PC.or (
                  PC.wrap (PC.seq (whitespace, PC.char #"}"),fn _ => Map.empty),
                  PC.seqWith map_together (
                    scanKeyValue,
                    PC.seqWith #1 (
                      PC.zeroOrMore (PC.seqWith #2 (PC.char #",",scanKeyValue)),
                      PC.eatChar (fn c => (c = #"}"))
                    )
                  )
                )
              ) r
      in
        scanValue r
      end


  val fromString = StringCvt.scanString scan

end
