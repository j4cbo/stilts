structure Startup = struct

  fun startup () = let
        val () = Command.conn := SOME (CLI.connect ("localhost", 9090))
        val cachedir = Command.cachedir ()
        val db = SQLite.opendb (cachedir ^ "/squeezecenter.db")
      in
        SQL.prepare db;
        (
          SearchFile.init "searchdb.idx"
          handle OS.SysErr _ => (
            print "Rebuilding index...\n";
            Index.prepare db;
            Index.generate "searchdb.idx";
            SearchFile.init "searchdb.idx"
          )
        )
      end

end
