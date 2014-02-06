structure SQLite :> SQLITE = struct

  type db_t = (ST_sqlite3.tag, C.rw) C.su_obj C.ptr'
  type db = db_t option ref
  type stmt_t = (ST_sqlite3_stmt.tag, C.rw) C.su_obj C.ptr'
  type stmt = stmt_t option ref

  datatype column_type = INTEGER | FLOAT | TEXT | BLOB | NULL
                       | UNKNOWN of int

  exception SQLiteClosed
  exception SQLiteError of string * int * string

  fun get (ref NONE) = raise SQLiteClosed
    | get (ref (SOME dbptr)) = dbptr

  val errmsg = ZString.toML' o F_sqlite3_errmsg.f' o get
  val errcode = Int32.toInt o F_sqlite3_errcode.f' o get

  fun opendb filename = let
       val filename' = ZString.dupML' filename
       val ptrcell = C.alloc' C.S.ptr 0w1
       val res = F_sqlite3_open.f' (filename', ptrcell)
       val ptr = C.Get.ptr' (C.Ptr.|*! ptrcell)
     in
       C.free' filename';
       C.free' ptrcell;
       if C.Ptr.isNull' ptr
         then raise SQLiteError ("sqlite3_open", Int32.toInt res, "")
         else ref (SOME ptr)
     end

  fun close db = (
        (F_sqlite3_close.f' (get db));
        db := NONE
      )

  fun prepare (db, query) = let
        val query' = ZString.dupML' query
        val stmtcell = C.alloc' C.S.ptr 0w1
        val res = F_sqlite3_prepare.f' (
                    get db, query', Int32.fromInt (size query),
                    stmtcell, C.Ptr.null');
        val _ = C.free' query'
        val ptr = C.Get.ptr' (C.Ptr.|*! stmtcell)
      in
        C.free' stmtcell;
        if C.Ptr.isNull' ptr
          then raise SQLiteError ("sqlite3_prepare",
                                  Int32.toInt res, errmsg db)
          else ref (SOME ptr)
      end

  val reset = ignore o F_sqlite3_reset.f' o get

  fun finalize stmt = (
        F_sqlite3_finalize.f' (get stmt);
        stmt := NONE
      )

  fun last_insert_rowid db = F_sqlite3_last_insert_rowid.f' (get db)
  fun changes db = F_sqlite3_changes.f' (get db)


  val SQLITE_TRANSIENT : C.voidptr = C.U.i2p (C.Cvt.c_ulong (~(0w1)))

  fun bind_blob (stmt : stmt, num, vec) = let
        val v' = ZString.dupML' (Byte.bytesToString vec)
      in
        Int32.toInt (F_sqlite3_bind_blob.f'
        (get stmt, Int32.fromInt num, C.Ptr.inject' v',
         Int32.fromInt (Word8Vector.length vec), SQLITE_TRANSIENT))
        before C.free' v'
     end

  fun bind_double (stmt, num, value) = Int32.toInt (
        F_sqlite3_bind_double.f' (get stmt, Int32.fromInt num, value))


  fun bind_int (stmt, num, value) = Int32.toInt (
        F_sqlite3_bind_int.f' (get stmt, Int32.fromInt num, value))

(*
  fun bind_int64 (stmt, num, value) = Int32.toInt (
        F_sqlite3_bind_int64.f' (get stmt, Int32.fromInt num, value))
*)
  fun bind_int64 _ = raise Fail "SML/NJ Int64 support is broken."

  fun bind_null (stmt, num) = Int32.toInt (
        F_sqlite3_bind_null.f' (get stmt, Int32.fromInt num))

  fun bind_text (stmt : stmt, num, str) = let
        val v' = ZString.dupML' str
      in
        Int32.toInt (F_sqlite3_bind_text.f'
        (get stmt, Int32.fromInt num, v',
         Int32.fromInt (size str), SQLITE_TRANSIENT))
        before C.free' v'
     end

  fun step stmt = Int32.toInt (F_sqlite3_step.f' (get stmt))

  val bytes = Int32.toInt o F_sqlite3_column_bytes.f'
  val ctype = F_sqlite3_column_type.f'

  val SQLITE_INTEGER : Int32.int = 1
  val SQLITE_FLOAT : Int32.int = 2
  val SQLITE_TEXT : Int32.int = 3
  val SQLITE_BLOB : Int32.int = 4
  val SQLITE_NULL : Int32.int = 5

  fun read_bytes (p, n) = Word8Vector.tabulate (n,
        fn i => Word8.fromLarge (Word32.toLarge (
                  C.Get.uchar' (C.Ptr.sub' C.S.uchar (p, i)))))

  fun column_blob (stmt, num) = let
        val arg = (get stmt, Int32.fromInt num)
        val f = F_sqlite3_column_blob.f'
      in
        if ctype arg = SQLITE_NULL
        then NONE
        else SOME (read_bytes (C.Ptr.cast' (f arg), bytes arg))
      end

  fun column_double (stmt, num) =
        F_sqlite3_column_double.f' (get stmt, Int32.fromInt num)

  fun column_int (stmt, num) =
        F_sqlite3_column_int.f' (get stmt, Int32.fromInt num)
(*
  fun column_int64 (stmt, num) =
        F_sqlite3_column_int64.f' (get stmt, Int32.fromInt num)
*)
  fun column_int64 _ = raise Fail "SML/NJ Int64 support is broken."

  fun column_text (stmt, num) = let
        val arg = (get stmt, Int32.fromInt num)
        val f = F_sqlite3_column_text.f'
      in
        if ctype arg = SQLITE_NULL
        then NONE
        else SOME (Byte.bytesToString (read_bytes (f arg, bytes arg)))
      end

  fun column_type (stmt, num) = case ctype (get stmt, Int32.fromInt num) of
        1 => INTEGER
      | 2 => FLOAT
      | 3 => TEXT
      | 4 => BLOB
      | 5 => NULL
      | i => UNKNOWN (Int32.toInt i)

end
