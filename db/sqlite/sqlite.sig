signature SQLITE = sig

  type db
  type stmt

  datatype column_type = INTEGER | FLOAT | TEXT | BLOB | NULL | UNKNOWN of int

  exception SQLiteClosed
  exception SQLiteError of int

  val errmsg : db -> string
  val errcode : db -> int

  val opendb : string -> db
  val close : db -> unit

  val prepare : db * string -> stmt
  val reset : stmt -> unit
  val finalize : stmt -> unit

  val bind_blob : stmt * int * Word8Vector.vector -> int
  val bind_double : stmt * int * Real64.real -> int
  val bind_int : stmt * int * Int32.int -> int
  val bind_int64 : stmt * int * Int64.int -> int
  val bind_null : stmt * int -> int
  val bind_text : stmt * int * string -> int

  val step : stmt -> int

  val column_blob : stmt * int -> Word8Vector.vector option
  val column_double : stmt * int -> Real64.real
  val column_int : stmt * int -> Int32.int
  val column_int64 : stmt * int -> Int64.int
  val column_text : stmt * int -> string option

  val column_type : stmt * int -> column_type

end
