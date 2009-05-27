structure Squall :> sig
  datatype backend = SQLite | MySQL
  type parsed_file = SquallInput.sqlfunc list
  val parse: string -> parsed_file
  val convert: parsed_file -> string
  val process: string -> string
  val process_and_write: string -> unit
  val main: 'a -> OS.Process.status
end = struct

  datatype backend = SQLite | MySQL

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


  (* val generateBindFunc: string * int * vartype -> string
   *
   * generateBindFunc typ produces SML source for a function that
   * will bind its input  its input to be inserted into an SQL statement.
   * This takes place after any option type handling.
   *
   * Example: generateEscapeFunc SI.Int  =>  "Int.toString"
   *)
  fun generateEscapeFunc (ivar, pos, SI.Int) =
        "SQLite3.bind_int (s, " ^ ivar ^ ", " ^ Int.toString pos ^ ")"
    | generateEscapeFunc (ivar, pos, SI.String) =
        "SQLite3.bind_text (s, " ^ ivar ^ ", " ^ Int.toString pos ^ ")"

  (* val generateBind: string * int * varspec -> string
   *
   * generateEscape (ivar, typ) produces SML source for an expression that will
   * bind the given variable to the given position in the query.
   *)
  fun generateEscape (ivar, pos, SI.Vrequired typ) = generateEscapeFunc (ivar, pos, typ)
    | generateEscape (ivar, pos, SI.Voption typ) =
          "(case " ^ ivar ^ " of NONE=>\"NULL\"|SOME " ^ ivar ^ "=>"
        ^ generateEscapeFunc (ivar, pos, typ) ^ ")"
    | generateEscape (ivar, pos, SI.Vlist typ) = raise Fail "h0no"

  (* val generateReader: int * vartype -> string
   *
   * generateReader (idx, typ) produces SML source for an expression that
   * will read out the given value from the statement "s".
   *)
  fun generateReader (idx, SI.Int) = "SQLite.column_int (s, " ^ Int.toString idx ^ ")"
    | generateReader (idx, SI.String) = "(case SQLite.column_text (s, " ^ Int.toString idx ^ ")of SOME s=>s|_=>raise(DataFormatError\""^Int.toString idx ^ "\"))"

  (* fun generateConv: int * varspec -> string
   *
   * generateConv (idx, typ) produces SML source for an expression that 
   * performs option checking on the given result column (if necessary) and
   * then converts it to the correct type.
   *)
  fun generateConv (idx, SI.Vrequired typ) = 
          "(case SQLite.column_type (s, " ^ Int.toString idx ^ ") of SQLite.NULL => "
        ^ "raise (DataFormatError\"" ^ Int.toString idx ^ "\") | _ => " 
        ^ generateReader (idx, typ) ^ ")"
    | generateConv (idx, SI.Voption typ) = 
          "(case SQLite.column_type (s, " ^ Int.toString idx ^ ") of SQLite.NULL => NONE | _ => SOME ("
        ^ generateReader (idx, typ) ^ "))"
    | generateConv (_, SI.Vlist typ) =
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
   e variables escaped and substituted. The expressions use free variables
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

      fun index nil = (nil, 0)
        | index ((a, b)::xs) = let val (xs', l) = index xs in ((a, l, b)::xs', l+1) end

      val (args', _) = index (rev args)
      
      val sql_gens = map generateEscape args'
    in
      (prologue, sql_gens)
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
  fun mkRepHandler SI.Rlist =   "      rev (get nil)\n"
    | mkRepHandler SI.Rarray =  "      Array.fromList (rev (get nil))\n"
    | mkRepHandler SI.Rvector = "      Vector.fromList (rev (get nil))\n"
    | mkRepHandler SI.Rsingle =
        "      case get nil of [ r ] => r\n"
      ^ "                    | x => raise Fail (\"Expected 1 row, got \"\n"
      ^ "                                       ^ (Int.toString (length x)))\n"
    | mkRepHandler SI.Roption =
        "      case get nil of nil => NONE\n"
      ^ "                   | [ r ] => SOME r\n"
      ^ "                   | x => raise Fail (\"Expected 0 or 1 rows, got \"\n"
      ^ "                                      ^ (Int.toString (length x)))\n"


  fun enumerate xs = rev (#2 (foldl (fn (x, (c, acc)) => (c+1, (x, c)::acc)) (0, nil) xs))

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
        ("()",
         "      case results of [] => ()\n"
       ^ "                    | _ => raise Fail \"Unexpected rows from DB\"\n")
    | mkOutputProc (SI.OBtuple (rt, types)) =
        let
          fun getCol (typ, i) = "\n            " ^ generateConv (i, typ)
        in
          ("(" ^ String.concatWith "," (map getCol (enumerate types))
           ^ "\n          )",
           mkRepHandler rt)
        end
    | mkOutputProc (SI.OBrecord (rt, args)) =
        let
          fun getCol ((name, typ), i) = "\n            " ^ name ^ "=" ^ generateConv (i, typ)
        in
          ("{" ^ String.concatWith "," (map getCol (enumerate args))
           ^ "\n          }",
           mkRepHandler rt)
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
      ^ "      val s = case STMTS." ^ name ^ " of ref (SOME s) => s | _ => raise Fail \"statement not prepared\"\n"
      ^ "      val () = checkAll ["
(*
      ^ String.concatWith "," (map (fn g => "\n        " ^ g) gens)
*)
      ^ "\n      ]\n"
      ^ "      fun get acc = case SQLite.step s of\n"
      ^ "        101 => acc\n"
      ^ "      | 100 => get (" ^ rowFun ^ "::acc)\n"
      ^ "      | i => raise Fail (\"unexpected step result \" ^ Int.toString i ^ \": \" ^ SQLite.errmsg (valOf (!STMTS.db)))\n"
      ^ "    in\n" ^ epilogue ^ "    end\n"
    end

  fun makePrepare { name, inb, outb, sql } =
        "\n    STMTS." ^ name ^ " := SOME (SQLite.prepare (db, \"" ^ String.toString sql ^ "\"))"

  fun makeStmtDecl { name, inb, outb, sql } =
        "    val " ^ name ^ " : SQLite.stmt option ref = ref NONE\n"

  fun makeRun { name, inb, outb, sql } = ""

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
  fun convert funcs = let
        val preps = map makePrepare funcs
        val runs = map makeRun funcs
      in
          "structure SQL = struct\n"
        ^ "  structure STMTS = struct\n"
        ^ "    val db : SQLite.db option ref = ref NONE\n"
        ^ String.concat (map makeStmtDecl funcs)
        ^ "  end\n"
        ^ "  val checkAll = List.app (fn i => if i=0 then()else raise Fail (\"bind: \" ^ Int.toString i))\n"
        ^ "  exception DataFormatError of string\n"
        ^ "  fun prepare db = (\n"
        ^ "    STMTS.db := SOME db;" ^ String.concatWith ";" preps ^ "\n  )\n"
        ^ (String.concat (map convertFunc funcs))
        ^ "end\n"
      end



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
