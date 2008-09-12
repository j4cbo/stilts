structure DB = struct

  val conn_info_root : MySQLClient.connect_info = {
        host = SOME "localhost", port = 0w0, unix_socket = NONE,
        user = SOME "root", password = NONE, db = SOME "slimserver"
      }

  val conn_info_sock : MySQLClient.connect_info = {
        host = NONE, port = 0w0, unix_socket = SOME "/var/lib/squeezecenter/cache/squeezecenter-mysql.sock",
        user = SOME "root", password = NONE, db = SOME "slimserver"
      }

  fun connect () = let
    val conn = MySQLClient.init ()
    val () = MySQLClient.real_connect conn conn_info_root
             handle MySQLClient.MySQLException _ =>
               MySQLClient.real_connect conn conn_info_sock
  in
    SQL.conn := SOME conn
  end

end
