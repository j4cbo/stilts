(* This is an SML/NJ CM tool that will automatically produce SML from .squall 
 * files.
 *)

structure SquallTool = struct

  fun squallRule { spec as { name, mkpath, class, opts, derived },
                   native2pathmaker, context, defaultClassOf, sysinfo } =
      let
        val srcpath = Tools.srcpath (mkpath ())
        val srcFile = (Tools.nativeSpec srcpath)
        val outputFile = srcFile ^ ".sml"

        val partial_expansion = (
              { smlfiles = nil,
                cmfiles = nil, 
                sources = [ (srcpath, { class = "squall", derived = false }) ] },
              [ { name = outputFile : string,
                  mkpath = native2pathmaker outputFile,
                  class = SOME "sml",
                  opts = NONE,
                  derived = true } ]
            )

        fun rulefun () = (
              if Tools.outdated "squall" ([ outputFile ], srcFile)
              then (Tools.vsay [ "[squall: compiling ", srcFile, "]\n" ];
                    Squall.process_and_write srcFile)
              else ();
              partial_expansion
            )
      in
        context rulefun
      end
   
  val _ = Tools.registerClass ("squall", squallRule)

  val _ = Tools.registerClassifier (Tools.SFX_CLASSIFIER (fn "squall" => SOME "squall" 
                                                           | _ => NONE))

end
