(* The ML-NLFFI generated code depends on this structure. It dynamically loads
   libmysqlclient on program startup, and provides hooks for the generated code
   to look up functions.
*)

structure Library = struct
  local
    val libs = [ "libmysqlclient.so", "libmysqlclient.so.15", "libmysqlclient.dylib" ]

    fun tryLib nil = raise Fail ("could not load any of "
                                 ^ (String.concatWith ", " libs))
      | tryLib (lib :: rest) = DynLinkage.open_lib { name = lib, global = true,
                                                    lazy = true }
                               handle x => tryLib rest

    val libHandle = tryLib libs

  in  
    fun libh sym = let
        val symHandle = DynLinkage.lib_symbol (libHandle, sym)
      in  
        fn () => DynLinkage.addr symHandle
      end
  end
end
