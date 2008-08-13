structure SmeltTool = struct

  fun smeltRule { spec as { name, mkpath, class, opts, derived },
                  native2pathmaker, context, defaultClassOf, sysinfo } =
      let
        val srcpath = Tools.srcpath (mkpath ())
        val srcFile = (Tools.nativeSpec srcpath)
        val outputFile = srcFile ^ ".sml"

        val partial_expansion = (
              { smlfiles = nil,
                cmfiles = nil, 
                sources = [ (srcpath, { class = "smelt", derived = false }) ] },
              [ { name = outputFile : string,
                  mkpath = native2pathmaker outputFile,
                  class = SOME "sml",
                  opts = NONE,
                  derived = true } ]
            )

        fun rulefun () = (
              if Tools.outdated "smelt" ([ outputFile ], srcFile)
              then (Tools.vsay [ "[smelt: compiling ", srcFile, "]\n" ];
                    Smelt.process_file srcFile)
              else ();
              partial_expansion
            )
      in
        context rulefun
      end
   
  val _ = Tools.registerClass ("smelt", smeltRule)

  val _ = Tools.registerClassifier (Tools.SFX_CLASSIFIER (fn "html" => SOME "smelt" 
                                                           | "smelt" => SOME "smelt" 
                                                           | _ => NONE))

end
