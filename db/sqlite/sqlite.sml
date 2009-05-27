structure SQLite :> SQLITE = struct

  structure P = MLton.Pointer

  datatype column_type = INTEGER | FLOAT | TEXT | BLOB | NULL | UNKNOWN of int

  type ptr = P.t

  type db = ptr option ref
  type stmt = ptr option ref

  type blob = unit
  type context = unit
  type file = unit
  type value = unit

  exception SQLiteClosed
  exception SQLiteError of string * int

  val () = MLton.Exn.addExnMessager (
    fn SQLiteError (s, i) => SOME ("SQLiteError: " ^ s ^ " returned " ^ Int.toString i)
     | _ => NONE)

  fun get (ref NONE) = raise SQLiteClosed
    | get (ref (SOME dbptr)) = dbptr

  fun copy_string (p: ptr) : string = let
        fun len offset = case P.getWord8 (p, offset) of
                           0w0 => offset
                         | _ => len (offset + 1)
      in
        Byte.bytesToString (Word8Vector.tabulate
                                         (len 0, fn i => P.getWord8 (p, i)))
      end


  val errmsg = copy_string o (_import "sqlite3_errmsg" : ptr -> ptr;) o get
  val errcode = (_import "sqlite3_errcode" : ptr -> int;) o get

  val malloc = _import "malloc" : C_Size.word -> ptr;
  val free = _import "free" : ptr -> unit;

  val sizeof_ptr = C_Size.fromInt (C_Size.wordSize div 8)

  fun opendb filename = let
        val openf = _import "sqlite3_open" : string * ptr -> int;
        val ptrcell = malloc sizeof_ptr
        val res = openf (filename ^ "\000", ptrcell)
        val pointer = P.getPointer (ptrcell, 0)
      in
        free ptrcell;
        if pointer = P.null
          then raise SQLiteError ("sqlite3_open", res)
          else ref (SOME pointer)
      end

  fun close db = (
        (_import "sqlite3_close" : ptr -> int;) (get db);
        db := NONE
      )

  fun prepare (db, query) = let
        val stmtcell = malloc sizeof_ptr
        val f = _import "sqlite3_prepare" : ptr * string * int * ptr * ptr -> int;
        val res = f (get db, query ^ "\000", size query, stmtcell, P.null);
        val pointer = P.getPointer (stmtcell, 0)
      in
        free stmtcell;
        if pointer = P.null
          then raise SQLiteError ("sqlite3_prepare", res)
          else ref (SOME pointer)
      end

  fun reset stmt = (_import "sqlite3_reset" : ptr -> unit;) (get stmt)

  fun finalize stmt = (
        (_import "sqlite3_finalize" : ptr -> int;) (get stmt);
        stmt := NONE
      )

  val SQLITE_TRANSIENT = P.sub (P.null, 0w1)

  fun bind_blob (stmt, num, vec) =
        (_import "sqlite3_bind_blob" : ptr * int * Word8Vector.vector * int * ptr -> int;)
        (get stmt, num, vec, Word8Vector.length vec, SQLITE_TRANSIENT)

  fun bind_double (stmt, num, value) =
        (_import "sqlite3_bind_blob" : ptr * int * Real64.real -> int;)
        (get stmt, num, value)

  fun bind_int (stmt, num, value) =
        (_import "sqlite3_bind_int" : ptr * int * Int32.int -> int;)
        (get stmt, num, value)

  fun bind_int64 (stmt, num, value) =
        (_import "sqlite3_bind_int" : ptr * int * Int64.int -> int;)
        (get stmt, num, value)

  fun bind_null (stmt, num) =
        (_import "sqlite3_bind_null" : ptr * int -> int;)
        (get stmt, num)

  fun bind_text (stmt, num, str) =
        (_import "sqlite3_bind_blob" : ptr * int * string * int * ptr -> int;)
        (get stmt, num, str, size str, SQLITE_TRANSIENT)

  fun step stmt = (_import "sqlite3_step" : ptr -> int;) (get stmt)

  val bytes = _import "sqlite3_column_bytes" : ptr * int -> int;
  val ctype = _import "sqlite3_column_type" : ptr * int -> int;

  val SQLITE_INTEGER = 1
  val SQLITE_FLOAT = 2
  val SQLITE_TEXT = 3
  val SQLITE_BLOB = 4
  val SQLITE_NULL = 5

  fun read_bytes (p, n) = Word8Vector.tabulate (n, fn i => P.getWord8 (p, i))

  fun column_blob (stmt, num) = let
        val arg = (get stmt, num)
        val f = _import "sqlite3_column_blob" : ptr * int -> ptr;
      in
        if ctype arg = SQLITE_NULL
        then NONE
        else SOME (read_bytes (f arg, bytes arg))
      end

  fun column_double (stmt, num) =
        (_import "sqlite3_column_double" : ptr * int -> Real64.real;)
        (get stmt, num)

  fun column_int (stmt, num) =
        (_import "sqlite3_column_double" : ptr * int -> Int32.int;)
        (get stmt, num)

  fun column_int64 (stmt, num) =
        (_import "sqlite3_column_double" : ptr * int -> Int64.int;)
        (get stmt, num)

  fun column_text (stmt, num) = let
        val arg = (get stmt, num)
        val f = _import "sqlite3_column_text" : ptr * int -> ptr;
      in
        if ctype arg = SQLITE_NULL
        then NONE
        else SOME (Byte.bytesToString (read_bytes (f arg, bytes arg)))
      end

  fun column_type (stmt, num) = case ctype (get stmt, num) of
        1 => INTEGER
      | 2 => FLOAT
      | 3 => TEXT
      | 4 => BLOB
      | 5 => NULL
      | i => UNKNOWN i

end
