structure Startup = struct

  fun startup () = let
        val () = Command.conn := SOME (CLI.connect ("localhost", 9090))
        val cachedir = Command.cachedir ()
(*
        val db = SQLite.opendb (cachedir ^ "/squeezecenter.db")
*)

        val conn_info_sock : MySQLClient.connect_info = {
              host = NONE, port = 0w0, unix_socket = SOME (cachedir ^ "/squeezebox-mysql.sock"),
              user = SOME "root", password = NONE, db = SOME "slimserver"
            }
        val db = MySQLClient.init ()
        val () = MySQLClient.real_connect db conn_info_sock
        val () = MySQLClient.set_reconnect db true

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
