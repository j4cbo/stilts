structure MySQLClient :> MYSQLCLIENT = struct

  (* Types and exceptions from MYSQLCLIENT *)

  type conn_t = (ST_st_mysql.tag, C.rw) C.su_obj C.ptr'
  type conn = conn_t option ref

  type result_t = (ST_st_mysql_res.tag, C.rw) C.su_obj C.ptr'
  type result' = (result_t * int * conn_t)
  type result = (result_t * int * conn) option ref

  type row_offset = C.voidptr

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


  (* Helper functions *)


  (* val raise_conn_error: conn_t -> 'a
   *
   * Call errno and errstr on the given connection and raise a MySQLException.
   *)
  fun raise_conn_error c' = let
                              val errno = F_mysql_errno.f' c'
                              val errstr = ZString.toML' (F_mysql_error.f' c')
                            in
                              print ("MySQL exception: \"" ^ errstr ^ "\"\n");
                              raise MySQLException(errno, errstr)
                            end


  (* val conn_wrapper: (conn_t -> 'a) -> conn -> 'a
   *
   * Verify that a connection is still open; then call the wrapped function
   * with the actual C connection object.
   *)
  fun conn_wrapper f ((ref NONE) : conn) = raise MySQLClosed
    | conn_wrapper f (ref (SOME c')) = f c'


  (* val res_wrapper: (result' -> 'a) -> result -> 'a
   *
   * Verify that the result has not been freed and that its connection is still
   * open; then call the wrapped function with the C connection and result
   * objects.
   *)
  fun res_wrapper f ((ref NONE) : result) = raise MySQLClosed
    | res_wrapper f (ref (SOME (r', i, ref NONE))) = raise MySQLClosed
    | res_wrapper f (ref (SOME (r', i, ref (SOME c')))) = f (r', i, c')


  (* val conn_wrapper_v: (conn_t * 'a -> 'b) -> conn -> 'a -> 'b
   * val res_wrapper_v: (result' * 'a -> 'b) -> result -> 'a -> 'b
   *
   * As conn_wrapper and res_wrapper, but each takes an additional value before
   * invoking the given function. These are necessary because they check the
   * connection _after_ the second curried argument is passed; something like
   * conn_wrapper (fn c' => fn v => ...) would allow partial application to
   * bypass the check.
   *)
  fun conn_wrapper_v f (c: conn) v = conn_wrapper (fn c' => f (c', v)) c
  fun res_wrapper_v f (r: result) v = res_wrapper (fn r' => f (r', v)) r


  (* val err_check: Int32.int -> unit
   *
   * Assert that the input value is 0. If not, call raise_conn_error to raise
   * the appropriate MySQLException.
   *)
  fun err_check (c', 0: Int32.int) = ()
    | err_check (c', _) = raise_conn_error c'


  fun cp NONE = C.Ptr.null'
    | cp (SOME s) = ZString.dupML' s

  fun fr p = if C.Ptr.isNull' p then () else C.free' p

  local
    open S_st_mysql_field
  in
    fun field_get fp = let
        val f = C.Ptr.|*! fp
      in { name = ZString.toML' (C.Get.ptr' (f_name' f)),
           org_name = ZString.toML' (C.Get.ptr' (f_org_name' f)),
           table = ZString.toML' (C.Get.ptr' (f_table' f)),
           org_table = ZString.toML' (C.Get.ptr' (f_org_table' f)),
           db = ZString.toML' (C.Get.ptr' (f_db' f)),
           catalog = ZString.toML' (C.Get.ptr' (f_catalog' f)),
           def = let val p = C.Get.ptr' (f_def' f)
                  in if C.Ptr.isNull' p then NONE
                                        else SOME (ZString.toML' p) end,
           length = C.Get.ulong' (f_length' f),
           max_length = C.Get.ulong' (f_max_length' f),
           flags = C.Get.uint' (f_flags' f),
           decimals = C.Get.uint' (f_decimals' f),
           charsetnr = C.Get.uint' (f_charsetnr' f) } : field
      end 
  end


  (* libmysqlclient *)

  fun my_init () = F_my_init.f' ()
 
  val affected_rows = conn_wrapper (fn c' =>
        Word64.fromLargeInt (Word32.toLargeInt (F_mysql_affected_rows.f' c')))

  val autocommit = conn_wrapper_v (fn (c', v) =>
        err_check (c', F_mysql_autocommit.f' (c', if v then 1 else 0)))

  val change_user = conn_wrapper_v (fn (c', { user, password, db }) =>
        let
          val u' = ZString.dupML' user
          val p' = ZString.dupML' password
          val db' = cp db
          val res = F_mysql_change_user.f' (c', u', p', db')
        in
          C.free' u' ;
          C.free' p' ;
          fr db';
          err_check (c', res)
        end) 

  val character_set_name = conn_wrapper (fn c' =>
        ZString.toML' (F_mysql_character_set_name.f' c'))

  fun close (ref NONE) = raise MySQLClosed
    | close (conn as ref (SOME c')) = (F_mysql_close.f' c'; conn := NONE)

  val commit = conn_wrapper (fn c' => err_check (c', F_mysql_commit.f' c'))

  fun data_seek _ =
        raise Fail "mysql_data_seek unimplemented due to SML/NJ limitations."

  fun debug s = let val s' = ZString.dupML' s
                 in F_mysql_debug.f' s';
                    C.free' s' end

  val dump_debug_info = conn_wrapper (fn c' =>
        err_check (c', F_mysql_dump_debug_info.f' c'))

  val errno = conn_wrapper F_mysql_errno.f'

  val error = conn_wrapper (fn c' => ZString.toML' (F_mysql_error.f' c'))

  val fetch_field = res_wrapper (fn (r', _, _) =>
        let val f = F_mysql_fetch_field.f' r'
         in if C.Ptr.isNull' f
            then NONE
            else SOME (field_get f)
        end)

  val fetch_field_direct = res_wrapper (fn (r', max, _) => fn i =>
        if i < max
        then field_get (F_mysql_fetch_field_direct.f' (r', Word32.fromInt i))
        else raise Subscript)


  fun fetch_lengths' (r', max, c') = let
        val lengths = F_mysql_fetch_lengths.f' r'
        val () = if C.Ptr.isNull' lengths then raise_conn_error c' else ()
        fun get n = if n >= max then nil
                    else C.Get.ulong' (C.Ptr.sub' C.S.ulong (lengths, n))
                         :: get (n+1)
      in
        get 0
      end

  val fetch_lengths = res_wrapper fetch_lengths'

  val fetch_row = res_wrapper (fn res' as (r', max, _) => let
        val row = F_mysql_fetch_row.f' r'
        fun get_field (colIndex, len) = let
            val base = C.Get.ptr' (C.Ptr.sub' C.S.ptr (row, colIndex))
          in
            if C.Ptr.isNull' base
            then NONE 
            else SOME (Byte.bytesToString (Word8Vector.tabulate (len, fn i =>
              UCharUtil.toWord8 (C.Get.uchar' (C.Ptr.sub' C.S.uchar (base, i)))
            )))
         end
      in
        if C.Ptr.isNull' row
        then NONE
        else SOME let
                    val indexes = List.tabulate (max, fn i => i)
                    val lengths = map Word32.toInt (fetch_lengths' res')
                  in       
                    map get_field (ListPair.zip (indexes, lengths))
                  end
      end)

  val field_seek = res_wrapper_v (fn ((r', _, _), i) =>
        Word32.toInt (F_mysql_field_seek.f' (r', (Word32.fromInt i))))

  val field_tell = res_wrapper (fn (r', _, _) =>
        Word32.toInt (F_mysql_field_tell.f' r'))

  fun free_result (ref NONE: result) = raise MySQLClosed
    | free_result (res as ref (SOME (r', _, _))) =
        (F_mysql_free_result.f' r'; res := NONE)

  val info = ZString.toML' o (conn_wrapper F_mysql_info.f')

  fun init () = ref (SOME (F_mysql_init.f' C.Ptr.null'))

  val insert_id = Word64.fromLargeInt
                o Word32.toLargeInt
                o (conn_wrapper F_mysql_insert_id.f')

  val num_rows = res_wrapper (fn (r', _, _) =>
        Word64.fromLargeInt (Word32.toLargeInt (F_mysql_num_rows.f' r')))

  val num_fields: result -> int = res_wrapper #2

  val ping = conn_wrapper (fn c' => err_check (c', F_mysql_ping.f' c'))

  val real_connect = conn_wrapper_v (fn (c', { host, user, password,
                                               db, port, unix_socket }) =>
    let
      val (h', u', p', db', us') =
          (cp host, cp user, cp password, cp db, cp unix_socket)
      val res = F_mysql_real_connect.f' (c', h', u', p', db', port, us', 0w0)
      val _ = (fr h', fr u', fr p', fr db', fr us')
    in
      if C.Ptr.isNull' res then raise_conn_error c' else ()
    end)

  val real_query = conn_wrapper_v (fn (c', s) =>
                     let
                       val s' = ZString.dupML' s
                       val len = Word32.fromInt (size s)
                       val res = F_mysql_real_query.f' (c', s', len)
                     in
                       C.free' s';
                       err_check (c', res)
                     end)

  fun real_escape_string (ref NONE) _ = raise MySQLClosed
    | real_escape_string (ref (SOME c')) s = let
          val src = ZString.dupML' s
          val dest_size = 2 * (size s) + 1
          val dest = C.alloc' C.S.uchar (Word.fromInt dest_size)
          val _ = F_mysql_real_escape_string.f' (c', dest, src,
                                                 Word32.fromInt (size s))
          val res = ZString.toML' dest
        in
          ZString.toML' dest before C.free' dest
        end

  val rollback = conn_wrapper (fn c' => err_check (c', F_mysql_rollback.f' c'))

  val row_seek = res_wrapper_v (fn ((r', _, _), i) =>
        F_mysql_row_seek.f' (r', i))

  val row_tell = res_wrapper (fn (r', _, _) => F_mysql_row_tell.f' r')

  val select_db = conn_wrapper_v (fn (c', s) =>
        let val s' = ZString.dupML' s
            val res = F_mysql_select_db.f' (c', s')
        in C.free' s'; err_check (c', res)
        end)

  val set_character_set = conn_wrapper_v (fn (c', s) =>
        let val s' = ZString.dupML' s
            val res = F_mysql_set_character_set.f' (c', s')
        in C.free' s'; err_check (c', res)
        end)

  val sqlstate = ZString.toML' o (conn_wrapper F_mysql_sqlstate.f')

  val ssl_set = conn_wrapper_v (fn (c', { key, cert, ca, capath, cipher }) =>
    let
      val (key', cert', ca', capath', cipher') =
          (cp key, cp cert, cp ca, cp capath, cp cipher)
      val res = F_mysql_ssl_set.f' (c', key', cert', ca', capath', cipher')
    in
      fr key'; fr cert'; fr ca'; fr capath'; fr cipher';
      err_check (c', res)
    end)

  val stat = ZString.toML' o (conn_wrapper F_mysql_stat.f')

  val thread_id = conn_wrapper F_mysql_thread_id.f'

  fun mk_store_use_result getter = (fn conn => case !conn of
          NONE => raise MySQLClosed
        | SOME c' => let
              val res = getter c'
            in
              if C.Ptr.isNull' res
              then NONE
              else SOME (ref (SOME (res,
                                    Word32.toInt (F_mysql_num_fields.f' res),
                                    conn)))
            end)

  val store_result = mk_store_use_result F_mysql_store_result.f'
  val use_result = mk_store_use_result F_mysql_use_result.f'


  fun query_and_result (conn, q) = (print q; real_query conn q;
                                    case store_result conn of
                                      NONE => []
                                    | SOME res => let
                                        fun get () = case fetch_row res of
                                                       NONE => nil
                                                     | SOME r => (r::(get ()))
                                      in
                                        (get () before free_result res)
                                        handle e => (free_result res; raise e)
                                      end)


end
