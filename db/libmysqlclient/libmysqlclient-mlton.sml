structure MySQLClient :> MYSQLCLIENT = struct

  (* Types and exceptions from MYSQLCLIENT *)

  type ptr = MLton.Pointer.t
  structure Ptr = MLton.Pointer

  type conn = ptr option ref

  type result_t = ptr
  type result' = (ptr * int * conn)
  type result = (ptr * int * conn) option ref

  type row_offset = ptr

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


  (* Imports *)
  val mysql_errno = _import "mysql_errno": ptr -> Word32.word;
  val mysql_error = _import "mysql_error": ptr -> ptr;

  (* Helper functions *)

  datatype long_size = L32 | L64
  val ls = case MLton.Platform.Arch.host of
                  MLton.Platform.Arch.AMD64 => L64
                | MLton.Platform.Arch.IA64 => L64
                | _ => L32


  fun cp NONE = Ptr.null
    | cp (SOME s) = let
           val malloc = _import "malloc": C_Size.word -> MLton.Pointer.t;
	   val mem = malloc (C_Size.fromInt (size s + 1))
	   fun copy (0, _) = ()
	     | copy (len, idx) = (Ptr.setWord8 (mem, idx, Byte.charToByte (String.sub (s, idx)));
	                          copy (len - 1, idx + 1))
	   val () = copy (size s, 0)
	   val () = Ptr.setWord8 (mem, size s, 0w0)
	 in
	   mem
	 end

  val fr = _import "free": MLton.Pointer.t -> unit;

  fun copy_string (p: ptr) : string = let
        fun len offset = case Ptr.getWord8 (p, offset) of
                           0w0 => offset
                         | _ => len (offset + 1)
      in
        Byte.bytesToString (Word8Vector.tabulate
	                                 (len 0, fn i => Ptr.getWord8 (p, i)))
      end


  (* val raise_conn_error: conn_t -> 'a
   *
   * Call errno and errstr on the given connection and raise a MySQLException.
   *)
  fun raise_conn_error c' = let
                              val errno = mysql_errno c'
                              val errstr = copy_string (mysql_error c')
                            in
                              print ("MySQL exception: \"" ^ errstr ^ "\"\n");
                              raise MySQLException(errno, errstr)
                            end


  fun n s = s ^ "\000"

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
  fun err_check_b (c', 0w0: Word8.word) = ()
    | err_check_b (c', _) = raise_conn_error c'

(*
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
*)

  (* libmysqlclient *)

  val my_init = _import "my_init": unit -> unit;

  val m_affected_rows = _import "mysql_affected_rows": ptr -> Word64.word;
  val affected_rows = conn_wrapper m_affected_rows

  val m_autocommit = _import "mysql_autocommit": ptr * Word8.word -> Word8.word;
  val autocommit = conn_wrapper_v (fn (c', v) =>
        err_check_b (c', m_autocommit (c', if v then 0w1 else 0w0)))

  val change_user = conn_wrapper_v (fn (c', { user, password, db }) =>
        let
          val f_string = _import "mysql_change_user": ptr * string * string * string -> Word8.word;
          val f_ptr = _import "mysql_change_user": ptr * string * string * ptr -> Word8.word;
          val res = case db of
                      SOME db => f_string (c', n user, n password, n db)
                    | NONE => f_ptr (c', n user, n password, MLton.Pointer.null)
        in
          err_check_b (c', res)
        end) 

  val character_set_name = conn_wrapper (fn c' =>
        copy_string ((_import "mysql_character_set_name": ptr -> ptr; ) c'))

  fun close (ref NONE) = raise MySQLClosed
    | close (conn as ref (SOME c')) =
        ((_import "mysql_close": ptr -> unit; ) c'; conn := NONE)

  val commit = conn_wrapper (fn c' =>
        err_check_b (c', (_import "mysql_commit": ptr -> Word8.word; ) c'))

  fun data_seek _ =
        raise Fail "mysql_data_seek unimplemented due to SML/NJ limitations."

  val debug = _import "mysql_debug": string -> unit;

  val dump_debug_info = conn_wrapper (fn c' =>
        err_check (c', (_import "mysql_dump_debug_info": ptr -> int; ) c'))

  val errno = conn_wrapper mysql_errno

  val error = conn_wrapper (copy_string o mysql_error)

  fun fetch_field _ = raise Fail "fetch_field not implemented"
  fun fetch_field_direct _ = raise Fail "fetch_field_direct not implemented"

  val getLong = case ls of L32 => Ptr.getWord32
                         | L64 => Word32.fromLargeWord o Ptr.getWord64

  fun fetch_lengths' (r', max, c') = let
        val lengths = (_import "mysql_fetch_lengths": ptr -> ptr; ) r'
        val () = if lengths = Ptr.null then raise_conn_error c' else ()
        fun get n = if n >= max then nil
                    else getLong (lengths, n) :: get (n + 1)
      in
        get 0
      end

  val fetch_lengths = res_wrapper fetch_lengths'

  val fetch_row = res_wrapper (fn res' as (r', max, _) => let
        val row = (_import "mysql_fetch_row": ptr -> ptr; ) r'
        fun get_field (colIndex, len) = let
            val base = Ptr.getPointer (row, colIndex)
          in
            if base = Ptr.null
            then NONE
            else SOME (Byte.bytesToString (Word8Vector.tabulate
	                                 (len, fn i => Ptr.getWord8 (base, i))))
         end
      in
        if row = Ptr.null
	then NONE
        else SOME let
                    val indexes = List.tabulate (max, fn i => i)
                    val lengths = map Word32.toInt (fetch_lengths' res')
                  in       
                    map get_field (ListPair.zip (indexes, lengths))
                  end
      end)

  val field_seek = res_wrapper_v (fn ((r', _, _), i) =>
        Word32.toInt ((_import "mysql_field_seek": ptr * Word32.word -> Word32.word; ) (r', (Word32.fromInt i))))

  val field_tell = res_wrapper (fn (r', _, _) =>
        Word32.toInt ((_import "mysql_field_tell": ptr -> Word32.word; ) r'))

  fun free_result (ref NONE: result) = raise MySQLClosed
    | free_result (res as ref (SOME (r', _, _))) =
        ((_import "mysql_free_result": ptr -> unit; ) r'; res := NONE)

  val info = conn_wrapper (copy_string o _import "mysql_info": ptr -> ptr; )

  fun init () = ref (SOME ((_import "mysql_init": ptr -> ptr; ) Ptr.null))

  val insert_id = conn_wrapper (_import "mysql_insert_id": ptr -> Word64.word; )

  val num_rows = res_wrapper (fn (r', _, _) => (_import "mysql_num_rows": ptr -> Word64.word; ) r')

  val num_fields: result -> int = res_wrapper #2

  val ping = conn_wrapper (fn c' => err_check (c', (_import "mysql_ping": ptr -> Int32.int; ) c'))

  val real_connect = conn_wrapper_v (fn (c', { host, user, password,
                                               db, port, unix_socket }) =>
    let
      val (h', u', p', db', us') =
          (cp host, cp user, cp password, cp db, cp unix_socket)
      val f = _import "mysql_real_connect": ptr * ptr * ptr * ptr * ptr * C_UInt.t * ptr * C_ULong.t -> ptr;
      val res = f (c', h', u', p', db', port, us', 0w0)
      val _ = (fr h', fr u', fr p', fr db', fr us')
    in
      if res = Ptr.null then raise_conn_error c' else ()
    end)

  val real_query = conn_wrapper_v (fn (c', s) =>
                     let
                       val f = _import "mysql_real_query": ptr * string * C_ULong.t -> Int32.int;
                     in
                       err_check (c', f (c', s, C_ULong.fromInt (size s)))
                     end)

  fun real_escape_string (ref NONE) _ = raise MySQLClosed
    | real_escape_string (ref (SOME c')) s = let
          val dest = Unsafe.CharArray.create (2 * (size s) + 1)
          val f = _import "mysql_real_escape_string": ptr * CharArray.array * string * C_ULong.t -> C_ULong.t;
          val outLen = C_ULong.toInt (f (c', dest, s, C_ULong.fromInt (size s)))
        in
          CharArraySlice.vector (CharArraySlice.slice (dest, 0, SOME outLen))
        end

  val rollback = conn_wrapper (fn c' => err_check_b (c', (_import "mysql_rollback": ptr -> Word8.word; ) c'))

  val row_seek = res_wrapper_v (fn ((r', _, _), i) =>
        (_import "mysql_row_seek": ptr * ptr -> ptr; ) (r', i))

  val row_tell = res_wrapper (fn (r', _, _) => (_import "mysql_row_tell": ptr -> ptr; ) r')

  val select_db = conn_wrapper_v (fn (c', s) =>
        err_check (c', (_import "mysql_select_db": ptr * string -> int; ) (c', n s)))

  val set_character_set = conn_wrapper_v (fn (c', s) =>
        err_check (c', (_import "mysql_set_character_set": ptr * string -> int; ) (c', n s)))

  val sqlstate = copy_string o (conn_wrapper _import "mysql_sqlstate": ptr -> ptr; )

  fun ssl_set _ = raise Fail "ssl_set unimplemented"

  val stat = copy_string o (conn_wrapper _import "mysql_state": ptr -> ptr; )

  val thread_id = Word32.fromInt o C_ULong.toInt o (conn_wrapper _import "mysql_thread_id": ptr -> C_ULong.t; )

  fun mk_store_use_result getter = (fn conn => case !conn of
          NONE => raise MySQLClosed
        | SOME c' => let
              val res = getter c'
	      val get_fields = Word32.toInt o _import "mysql_num_fields": ptr -> Word32.word;
            in
              if res = Ptr.null
	      then NONE
              else SOME (ref (SOME (res, get_fields res, conn)))
            end)

  val store_result = mk_store_use_result _import "mysql_store_result": ptr -> ptr;
  val use_result = mk_store_use_result _import "mysql_use_result": ptr -> ptr;

  fun query_and_result (conn, q) = let
        val sp = String.translate (fn #"\n" => "\n       " | c => String.str c)
        val () = print ("MySQL: Running query: \n       " ^ sp q ^ "\n")

        val timer = Timer.startRealTimer ()
        val () = real_query conn q
        val queryTime = Time.toReal (Timer.checkRealTimer timer)
        val queryTS = Real.toString (queryTime * 1000.0)

        val res = store_result conn
        val storeTime = Time.toReal (Timer.checkRealTimer timer)
        val storeTS = Real.toString ((storeTime - queryTime) * 1000.0)

        fun get res = case fetch_row res of NONE => nil
                                          | SOME r => (r::(get res))
      in
        case res of NONE => (print ("MySQL: done; query " ^ queryTS
                                    ^ " ms, store " ^ storeTS ^ " ms\n");
                             [])
                  | SOME res => let
                      val resultSet = (
                        (get res before free_result res)
                        handle e => (free_result res; raise e)
                      )
                      val loadTime = Time.toReal (Timer.checkRealTimer timer)
                      val loadTS = Real.toString ((loadTime-storeTime)*1000.0)
                      val totalTS = Real.toString (loadTime * 1000.0)
                    in
                      print ("MySQL: done; query " ^ queryTS ^ " ms, store "
                             ^ storeTS ^ " ms, load " ^ loadTS ^ " ms, total "
                             ^ totalTS ^ " ms\n");
                      resultSet
                    end
      end
end
