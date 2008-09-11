structure Squall :> sig
  type parsed_file = SquallInput.sqlfunc list
  val parse: string -> parsed_file
  val convert: parsed_file -> string
  val process: string -> string
  val process_and_write: string -> unit
  val main: 'a -> OS.Process.status
end = struct

  structure SquallLrVals = SquallLrValsFun(structure Token = LrParser.Token)
  structure SquallLex = SquallLexFun(structure Tokens = SquallLrVals.Tokens)
  structure SquallParser = Join(structure Lex = SquallLex
                                structure LrParser = LrParser
                                structure ParserData = SquallLrVals.ParserData)

  structure SI = SquallInput

  type parsed_file = SI.sqlfunc list

  (* val parse: string -> SI.sqlfunc list
   *
   * Load and parse the specified file.
   *)
  fun parse s =
      let val dev = TextIO.openIn s
          val stream = SquallParser.makeLexer(fn i => TextIO.inputN(dev, i))
          fun error (e, i:int, _) =
             TextIO.output(TextIO.stdOut, s ^ "," ^ " line " ^ (Int.toString i)
                                            ^ ", Error: " ^ e ^ "\n")
          val () = SquallLex.UserDeclarations.lineNum := 1
          val (res, _) = SquallParser.parse(30,stream,error,())
          val () = TextIO.closeIn dev
      in res
      end


  (* val mlquote: string -> string
   *
   * Escape the given string for use as a literal in SML source.
   * XXX: This is probably not complete.
   *)
  fun mlquote s = "\"" ^ (String.translate (fn #"\"" => "\\\""
                                             | #"\\" => "\\\\"
                                             | #"\n" => "\\n"
                                             | c => String.str c) s) ^ "\""


  (* val generateEscapeFunc: vartype -> string
   *
   * generateEscapeFunc typ produces SML source for a function that
   * will escape its input to be inserted into an SQL statement.
   * This takes place after any option type handling.
   *
   * Example: generateEscapeFunc SI.Int  =>  "Int.toString"
   *)
  fun generateEscapeFunc SI.Int = "Int.toString"
    | generateEscapeFunc SI.String =
          "(fn v=>\"\\\"\"^MySQLClient.real_escape_string conn v^\"\\\"\")"

  (* val generateEscape: string * varspec -> string
   *
   * generateEscape (ivar, typ) produces SML source for an expression that will
   * escape the given free variable, or variable option, for SQL.
   *)
  fun generateEscape (ivar, SI.Vrequired typ) =
        "(" ^ generateEscapeFunc typ ^ " " ^ ivar ^ ")"
    | generateEscape (ivar, SI.Voption typ) =
          "(case " ^ ivar ^ " of NONE=>\"NULL\"|SOME " ^ ivar ^ "=>"
        ^ generateEscapeFunc typ ^ " " ^ ivar ^ ")"
    | generateEscape (ivar, SI.Vlist typ) =
          "(\"(\"^String.concatWith\",\"(map " ^ generateEscapeFunc typ
        ^ " " ^ ivar ^ ")^\")\")"

  (* val generateValueConv: string * vartype -> string
   *
   * generateValueConv (ivar, typ) produces SML source for an expression that
   * will turn the given free string variable into a value of the correct type.
   *
   * Example: generateEscape ("foo", SI.String)  =>  "foo"
   *)
  fun generateValueConv (ivar, SI.Int) =
          "(case Int.fromString " ^ ivar
        ^ " of SOME i => i | NONE => raise (DataFormatError \""
        ^ (String.toString ivar) ^ "\"))"
    | generateValueConv (ivar, SI.String) = ivar


  (* fun generateConv: string * varspec -> string
   *
   * generateConv (ivar, typ) produces SML source for an expression that 
   * performs option checking on the given free variable (if necessary) and
   * then converts it to the correct type.
   *)
  fun generateConv (ivar, SI.Vrequired typ) = 
          "(case " ^ ivar ^ " of NONE=>raise (DataFormatError\""
        ^ (String.toString ivar) ^ "\")|SOME " ^ ivar ^ "=>"
        ^ (generateValueConv (ivar, typ)) ^ ")"
    | generateConv (ivar, SI.Voption typ) = 
          "(case " ^ ivar ^ " of NONE=>NONE|SOME " ^ ivar ^ "=>SOME "
        ^ (generateValueConv (ivar, typ)) ^ ")"
    | generateConv (ivar, SI.Vlist typ) =
          raise Fail "list not allowed in output types" 


  (* val mkSQLGen: SI.inbinding * string -> string * string list
   *
   * Produce a tuple (prologue: string, gens: string list) for the given input
   * binding and SQL.
   *
   * Prologue is a pattern-match, such as {x} or (foo, bar), for use in a
   * function expression.
   *
   * Gens is a list of SML code snippets, each evaluating to a string, which
   * when concatenated will produce a version of the input SQL with all ?
   * variables escaped and substituted. The expressions use free variables
   * provided by prologue.
   *)
  fun mkSQLGen (inb, sql) = let

      fun mix (x::xs) ys = x::(mix' xs ys)
        | mix nil _ = raise ListPair.UnequalLengths
      and mix' nil nil = nil
        | mix' xs (y::ys) = y::(mix xs ys)
        | mix' _ nil = raise ListPair.UnequalLengths
      
      val (prologue, args) = case inb of 
            SI.IBunit => ("()", nil)
          | SI.IBtuple arg_types => let
                val arg_names = List.tabulate (length arg_types,
                                               fn i => "a_" ^ Int.toString i);
                val prologue = "(" ^ (String.concatWith "," arg_names) ^ ")"
              in (prologue, ListPair.zip (arg_names, arg_types)) end
          | SI.IBrecord args => let
                val prologue = "{" ^ (String.concatWith "," (map #1 args)) ^ "}"
              in (prologue, args) end
      
      val sql_sections = map (fn s => "\"" ^ String.toString s ^ "\"")
                             (String.fields (fn c => c = #"?") sql)
      val sql_gens = map generateEscape args

    in
      (prologue, mix sql_sections sql_gens)
      handle ListPair.UnequalLengths =>
        raise Fail ("expected " ^ (Int.toString ((length sql_sections) - 1))
                    ^ " args, got " ^ (Int.toString (length args)))
    end


  (* val mkRepHandler: SI.reptype -> string
   *
   * Produce an expression that will turn results: string list list into the
   * appropriate output value ('a list, 'a option, etc.). This ensures that the
   * correct multiplicity of rows were returned: 0 for functions returning
   * unit, 1 if returning a single tuple/record directy, etc.
   *
   * The resultant code assumes the existence of a function row:
   * string list -> 'a, which handles converting each row; the row function is
   * generated by mkOutputProc.
   *
   * This is used by mkOutputProc for the OBtuple and OBrecord options, but not
   * for OBunit.
   *)
  fun mkRepHandler SI.Rlist =   "      map row results\n"
    | mkRepHandler SI.Rarray =  "      Array.fromList (map row results)\n"
    | mkRepHandler SI.Rvector = "      Vector.fromList (map row results)\n"
    | mkRepHandler SI.Rsingle =
        "      case results of [ r ] => row r\n"
      ^ "                    | x => raise Fail (\"Expected 1 row, got \"\n"
      ^ "                                       ^ (Int.toString (length x)))\n"
    | mkRepHandler SI.Roption =
        "      case results of nil => NONE\n"
      ^ "                   | [ r ] => SOME (row r)\n"
      ^ "                   | x => raise Fail (\"Expected 0 or 1 rows, got \"\n"
      ^ "                                      ^ (Int.toString (length x)))\n"


  (* val mkOutputProc: SI.outbinding -> string * string
   *
   * Build two snippets of SML code for the output procedure. The first is a
   * declaration of any additional context needed for the output expression;
   * the second is an expression to be evaluated as the function's output.
   *
   * For OBunit, no additional declarations are needed. The output expression
   * simply asserts that the result set contains no rows. OBtuple and OBrecord
   * produce a function "row", of type string list -> (...) or string list ->
   * {...}, respectively; the final expression is generated by mkRepHandler to
   * build a suitable collection of all the rows.
   *)
  fun mkOutputProc SI.OBunit =
        ("",
         "      case results of [] => ()\n"
       ^ "                    | _ => raise Fail \"Unexpected rows from DB\"\n")
    | mkOutputProc (SI.OBtuple (rt, types)) =
        let
          val names = List.tabulate(length types, fn i => "a_" ^ Int.toString i)
          val conversions = map generateConv (ListPair.zip (names, types))
          val rowFun = "      fun row [" ^ (String.concatWith ", " names)
                     ^ "] = (" ^ (String.concatWith ", " conversions)
                     ^ ")\n        | row x = raise Fail (\"Expected "
                     ^ (Int.toString (length types))
                     ^ " columns, got \" ^ (Int.toString (length x)))\n"
        in
          (rowFun, mkRepHandler rt) 
        end
    | mkOutputProc (SI.OBrecord (rt, args)) = 
        let
          val arg_names = map #1 args
          fun genBinding (arg as (ivar, typ)) = ivar ^ "=" ^ (generateConv arg)
          val arg_conversions = map genBinding args

          val rowFun = "      fun row [" ^ (String.concatWith ", " arg_names)
                     ^ "] = {" ^ (String.concatWith ", " arg_conversions)
                     ^ "}\n        | row x = raise Fail (\"Expected "
                     ^ (Int.toString (length args))
                     ^ " columns, got \" ^ (Int.toString (length x)))\n"
        in
          (rowFun, mkRepHandler rt)
        end


  (* val convertFunc: SI.sqlfunc -> string
   *
   * This wraps the output of the above functions into a single function
   * declaration. The result has the following basic form:
   *
   * fun [name] [params] = let                <- unit, (tuple), { record }
   *     val conn = (get_conn_value)          <- raise Fail if no connection
   *     val results = MySQL.query_and_result
   *           (conn, String.concat ... )     <- query code built by mkSQLGen
   *     fun row [ ... ] = ...                <- convert each row to the
   *   in                                        appropriate tuple or record
   *     (map) row results                    <- depends on output type/reptype
   *   end
   *)
  fun convertFunc { name, inb, outb, sql } =
    let
      val (prologue, gens) = mkSQLGen (inb, sql)
      val (rowFun, epilogue) = mkOutputProc outb
    in  
        "  fun " ^ name ^ " " ^ prologue
      ^ " = let\n"
      ^ "      val conn = case !conn of SOME c => c"
      ^ " | NONE => raise Fail \"No database connection.\"\n"
      ^ "      val results = MySQLClient.query_and_result (conn, "
      ^ (case gens of [s] => (s ^ ")\n")
                    | gens => ("String.concat [\n        "      
                               ^ (String.concatWith ",\n        " gens)
                               ^ "\n        ])\n"))
      ^ rowFun
      ^ "    in\n" ^ epilogue ^ "    end\n"
    end


  (* val convert: SI.sqlfunc list -> string
   *
   * Process a set of query definitions and produce a single module containing
   * all of them.
   *
   * The module stores a reference internally to its connection. At startup,
   * this is NONE, and so attempting to use any query function will raise Fail.
   * One must first set up the connection:
   *
   *   SQL.conn := SOME c
   *)
  fun convert funcs = "structure SQL = struct\n"
                    ^ "  exception DataFormatError of string\n"
                    ^ "  val conn: MySQLClient.conn option ref = ref NONE\n"
                    ^ (String.concat (map convertFunc funcs))
                    ^ "end\n"



  (* val process: string -> string
   *
   * Load definitions from a file and process them as above. 
   *)
  val process = convert o parse


  (* val main: 'a -> OS.process.status
   *
   * Main function.
   *)

  fun err msg = TextIO.output(TextIO.stdErr, String.concat msg)

  fun process_and_write filename = let
        val result = process filename
        val outfile = TextIO.openOut (filename ^ ".sml")
      in
        TextIO.output (outfile, result);
        TextIO.closeOut outfile
      end

  fun main _ = (case CommandLine.arguments () of
                 [ filename ] => (process_and_write filename;
                                  OS.Process.success)
               | _ => (
                   print ("Usage: " ^ (CommandLine.name ()) ^ " squallfile\n");
                   OS.Process.failure
                 ))

              handle e => (err [ CommandLine.name(), ": uncaught exception ",
                                 General.exnMessage e, "\n"];
                           OS.Process.failure)
end
