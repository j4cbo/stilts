signature MYSQLCLIENT = sig

  (* This is a fairly direct translation of the MySQL C API. Note that
   * functions not used by Squall may be untested.
   *
   * The function "query_and_result" is an added helper. It performs a query,
   * retrieves all the results, and then frees the result set; this uses
   * real_query, store_result, and free_result. If the query returned no result
   * set (store_result returns NONE), query_and_result returns nil.
   *)

  type conn
  type result
  type row_offset

  type my_ulonglong = Word64.word

  type field = { name: string,
                 org_name: string, 
                 table: string,
                 org_table: string,
                 db: string,
                 catalog: string,
                 def: string option,
                 length: Word32.word,
                 max_length: Word32.word,
                 flags: Word32.word,
                 decimals: Word32.word,
                 charsetnr: Word32.word }

  type connect_info = { host: string option,
                        user: string option,
                        password: string option,
                        db: string option,
                        port: Word32.word,
                        unix_socket: string option }

  exception MySQLException of Word32.word * string
  exception MySQLClosed

  val my_init: unit -> unit

  val affected_rows: conn -> my_ulonglong
  val autocommit: conn -> bool -> unit
  val change_user: conn -> { user: string,
                             password: string,
                             db: string option } -> unit
  val character_set_name: conn -> string
  val close: conn -> unit
  val commit: conn -> unit
  val data_seek: result -> my_ulonglong -> unit
  val debug: string -> unit
  val dump_debug_info: conn -> unit
  val errno: conn -> Word32.word
  val error: conn -> string
  val fetch_field: result -> field option
  val fetch_field_direct: result -> int -> field
  val fetch_lengths: result -> Word32.word list
  val fetch_row: result -> string option list option
  val field_seek: result -> int -> int
  val field_tell: result -> int 
  val free_result: result -> unit
  val info: conn -> string
  val init: unit -> conn
  val insert_id: conn -> my_ulonglong
  val num_fields: result -> int
  val num_rows: result -> my_ulonglong
  val ping: conn -> unit
  val real_connect: conn -> connect_info -> unit
  val real_escape_string: conn -> string -> string
  val real_query: conn -> string -> unit
  val rollback: conn -> unit
  val row_seek: result -> row_offset -> row_offset
  val row_tell: result -> row_offset
  val select_db: conn -> string -> unit
  val set_character_set: conn -> string -> unit
  val sqlstate: conn -> string
  val ssl_set: conn -> { key: string option,
                         cert: string option, 
                         ca: string option,
                         capath: string option,
                         cipher: string option } -> unit
  val stat: conn -> string
  val store_result: conn -> result option
  val thread_id: conn -> Word32.word
  val use_result: conn -> result option

  val set_reconnect: conn -> bool -> unit

  val query_and_result: conn * string -> string option list list

end
