structure Smelt = struct

  structure XP = TinyXMLParser
  structure TC = TemplateCompiler
  structure TG = TemplateGenerator

  fun err msg = TextIO.output (TextIO.stdErr, String.concat msg)

  fun process_file filename = let
        val template = XP.parseFile filename
        val result = (TC.compile o TG.generate) template
        val outfile = TextIO.openOut (filename ^ ".sml")
      in
        TextIO.output (outfile, result);
        TextIO.closeOut outfile
      end

  fun main _ = (case CommandLine.arguments () of
                 [ filename ] => (process_file filename; OS.Process.success)
               | _ => (print ("Usage: " ^ (CommandLine.name ())
                                        ^ " templatefile\n");
                   OS.Process.failure
                 ))
              handle e => (err [ CommandLine.name(), ": uncaught exception ",
                                 General.exnMessage e, "\n"];
                           OS.Process.failure)

end
