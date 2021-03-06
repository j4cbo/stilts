structure SquallSQLite :> sig
  val convert: SquallInput.sqlfunc list -> string
end = struct

  structure SI = SquallInput

  (* val generateBindFunc: string * int * vartype -> string
   *
   * generateBindFunc typ produces SML source for a function that
   * will bind its input to be inserted into an SQL statement.
   * This takes place after any option type handling.
   *)
  fun generateBindFunc (ivar, pos, SI.Int) =
        "SQLite.bind_int (s, " ^ Int.toString (pos + 1) ^ ", " ^ ivar ^ ")"
    | generateBindFunc (ivar, pos, SI.String) =
        "SQLite.bind_text (s, " ^ Int.toString (pos + 1) ^ ", " ^ ivar ^ ")"
    | generateBindFunc (ivar, pos, SI.Blob) =
        "SQLite.bind_blob (s, " ^ Int.toString (pos + 1) ^ ", " ^ ivar ^ ")"


  (* val generateBind: string * int * varspec -> string
   *
   * generateBind (ivar, typ) produces SML source for an expression that will
   * bind the given variable to the given position in the query.
   *)
  fun generateBind (ivar, pos, SI.Vrequired typ) =
        generateBindFunc (ivar, pos, typ)
    | generateBind (ivar, pos, SI.Voption typ) =
          "(case " ^ ivar ^ " of NONE => SQLite.bind_null (s, "
        ^ Int.toString (pos + 1) ^ ") | SOME " ^ ivar ^ " => "
        ^ generateBindFunc (ivar, pos, typ) ^ ")"
    | generateBind (ivar, pos, SI.Vlist typ) =
        raise Fail ("The SQLite backend only supports list inputs if the sole"
                  ^ " input into the statement is an 'int list'.")


  (* val generateReader: int * vartype -> string
   *
   * generateReader (idx, typ) produces SML source for an expression that
   * will read out the given value from the statement "s".
   *)
  fun generateReader (idx, SI.Int) = "SQLite.column_int (s, " ^ Int.toString idx ^ ")"
    | generateReader (idx, SI.String) = "(case SQLite.column_text (s, " ^ Int.toString idx ^ ")of SOME s=>s|_=>raise(DataFormatError\""^Int.toString idx ^ "\"))"
    | generateReader (idx, SI.Blob) = "(case SQLite.column_blob (s, " ^ Int.toString idx ^ ")of SOME s=>s|_=>raise(DataFormatError\""^Int.toString idx ^ "\"))"

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

      val sql_gens = map generateBind args'
    in
      (prologue, sql_gens)
    end


  (* val mkRepHandler: SI.reptype -> string
   *
   * Produce an expression that will turn results: 'a list into the
   * appropriate output value ('a list, 'a option, etc.). This ensures that the
   * correct multiplicity of rows were returned: 0 for functions returning
   * unit, 1 if returning a single tuple/record directy, etc.
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
    | mkRepHandler SI.Rfold =
        "      get\n"


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
         "      case get nil of [] => ()\n"
       ^ "                    | _ => raise Fail \"Unexpected rows from DB\"\n")
    | mkOutputProc SI.OBinsertId =
        ("()",
         "      case get nil of [] => SQLite.last_insert_rowid (valOf (!STMTS.db))\n"
       ^ "                    | _ => raise Fail \"Unexpected rows from DB\"\n")
    | mkOutputProc SI.OBaffectedRows =
        ("()",
         "      case get nil of [] => SQLite.changes (valOf (!STMTS.db))\n"
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
  fun mkExtra outb rowFun = let
        val isFold = case outb of SI.OBtuple (SI.Rfold, _) => true
                                | SI.OBrecord (SI.Rfold, _) => true
                                | _ => false
      in
        (isFold, if isFold then "f (" ^ rowFun ^ ", acc)" else rowFun ^ "::acc")
      end

  fun convertFunc { name, inb as SI.IBtuple [ SI.Vlist SI.Int ], outb, sql } =
    let
      val (s1, s2) = case String.fields (fn c => c = #"?") sql of
                       [ a, b ] => (a, b)
                     | _ => raise Fail "expected exactly one ?"
      val (rowFun, epilogue) = mkOutputProc outb
      val (isFold, getRecurs) = mkExtra outb rowFun
    in
        "  fun " ^ name ^ " vs " ^ (if isFold then "f " else "") ^ "= let\n"
      ^ "      val db = case STMTS.db of ref (SOME db) => db | _ => raise Fail \"database not available\"\n"
      ^ "      val s = SQLite.prepare (db, \"" ^ String.toString s1
      ^ "(\" ^ String.concatWith \",\" (map Int.toString vs) ^ \")"
      ^ String.toString s2 ^ "\")\n"
      ^ "      fun get acc = case SQLite.step s of\n"
      ^ "        101 => acc\n"
      ^ "      | 100 => get (" ^ getRecurs ^ ")\n"
      ^ "      | i => raise Fail (\"unexpected step result \" ^ Int.toString i ^ \": \" ^ SQLite.errmsg (valOf (!STMTS.db)))\n"
      ^ "    in\n(" ^ epilogue ^ ") before SQLite.finalize s end\n"
    end

    | convertFunc { name, inb, outb, sql } =
    let
      val (prologue, gens) = mkSQLGen (inb, sql)
      val (rowFun, epilogue) = mkOutputProc outb
      val (isFold, getRecurs) = mkExtra outb rowFun
    in
        "  fun " ^ name ^ " " ^ prologue ^ (if isFold then " f" else "")
      ^ " = let\n"
      ^ "      val s = case STMTS." ^ name ^ " of ref (SOME s) => s | _ => raise Fail \"statement not prepared\"\n"
      ^ "      val _ = SQLite.reset s\n"
      ^ "      val () = checkAll ["
      ^ String.concatWith "," (map (fn g => "\n        " ^ g) gens)
      ^ "\n      ]\n"
      ^ "      fun get acc = case SQLite.step s of\n"
      ^ "        101 => acc\n"
      ^ "      | 100 => get (" ^ getRecurs ^ ")\n"
      ^ "      | i => raise Fail (\"unexpected step result \" ^ Int.toString i ^ \": \" ^ SQLite.errmsg (valOf (!STMTS.db)))\n"
      ^ "    in\n" ^ epilogue ^ "    end\n"
    end

  fun makePrepare { name, inb as SI.IBtuple [ SI.Vlist SI.Int ], outb, sql }=""
    | makePrepare { name, inb, outb, sql } =
        "\n    STMTS." ^ name ^ " := SOME (SQLite.prepare (db, \"" ^ String.toString sql ^ "\"));"

  fun makeStmtDecl { name, inb as SI.IBtuple [ SI.Vlist SI.Int ], outb, sql }=""
    | makeStmtDecl { name, inb, outb, sql } =
        "    val " ^ name ^ " : SQLite.stmt option ref = ref NONE\n"

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
      in
          "structure SQL = struct\n"
        ^ "  structure STMTS = struct\n"
        ^ "    val db : SQLite.db option ref = ref NONE\n"
        ^ String.concat (map makeStmtDecl funcs)
        ^ "  end\n"
        ^ "  val checkAll = List.app (fn i => if i=0 then()else raise Fail (\"bind: \" ^ Int.toString i))\n"
        ^ "  exception DataFormatError of string\n"
        ^ "  fun prepare db = ("
        ^ String.concat preps ^ "\n    STMTS.db := SOME db\n  )\n"
        ^ (String.concat (map convertFunc funcs))
        ^ "end\n"
      end

end
