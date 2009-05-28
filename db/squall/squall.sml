structure Squall :> sig

  type parsed_file = SquallInput.engine option * SquallInput.sqlfunc list

  val parse : string -> parsed_file
  val convert : SquallInput.engine option -> parsed_file -> string
  val process : SquallInput.engine option -> string -> string
  val process_and_write: SquallInput.engine option -> string -> unit
  val main: 'a -> OS.Process.status

end = struct

  structure SquallLrVals = SquallLrValsFun(structure Token = LrParser.Token)
  structure SquallLex = SquallLexFun(structure Tokens = SquallLrVals.Tokens)
  structure SquallParser = Join(structure Lex = SquallLex
                                structure LrParser = LrParser
                                structure ParserData = SquallLrVals.ParserData)

  structure SI = SquallInput

  type parsed_file = SquallInput.engine option * SquallInput.sqlfunc list

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


  fun convertWith SI.SQLite funcs = SquallSQLite.convert funcs
    | convertWith SI.MySQL funcs = SquallMySQL.convert funcs

  fun convert (SOME e) (NONE, funcs) = convertWith e funcs
    | convert NONE (SOME e, funcs) = convertWith e funcs
    | convert NONE (NONE, funcs) = raise Fail "Must specify an engine."
    | convert (SOME e1) (SOME e2, funcs) = if e1 = e2 then convertWith e1 funcs
                                   else raise Fail "Conflicting engines in command line and file."

  (* val process: string -> string
   *
   * Load definitions from a file and process them as above.
   *)
  fun process engine filename = convert engine (parse filename)


  fun err msg = TextIO.output(TextIO.stdErr, String.concat msg)

  fun process_and_write engine filename = let
        val result = process engine filename
        val outfile = TextIO.openOut (filename ^ ".sml")
      in
        TextIO.output (outfile, result);
        TextIO.closeOut outfile
      end

  (* val main: 'a -> OS.process.status
   *
   * Main function.
   *)

  fun main _ = (case CommandLine.arguments () of
                 [ filename ] => (process_and_write NONE filename;
                                  OS.Process.success)
               | [ "--sqlite", filename ] => (process_and_write (SOME SI.SQLite) filename;
                                  OS.Process.success)
               | [ "--mysql", filename ] => (process_and_write (SOME SI.MySQL) filename;
                                  OS.Process.success)
               | _ => (
                   print ("Usage: " ^ (CommandLine.name ()) ^ " [--sqlite | --mysql] squallfile\n");
                   OS.Process.failure
                 ))

              handle e => (err [ CommandLine.name(), ": uncaught exception ",
                                 General.exnMessage e, "\n"];
                           OS.Process.failure)
end
