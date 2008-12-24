structure HTTPDate :> sig

  (* Format dates in HTTP format.
   *
   * We cannot use the Date.fmt function in the basis library for this, because
   * it uses the current locale (and the current locale's names for days of
   * week and months), rather than the universal RFC 1123 date format.
   *)

  val format: Date.date -> string

end = struct
 
  structure D = Date
  val format_wd = fn D.Mon => "Mon" | D.Tue => "Tue" | D.Wed => "Wed"
                   | D.Thu => "Thu" | D.Fri => "Fri" | D.Sat => "Sat"
                   | D.Sun => "Sun"

  val format_mon = fn D.Jan => "Jan" | D.Feb => "Feb" | D.Mar => "Mar"
                    | D.Apr => "Apr" | D.May => "May" | D.Jun => "Jun"
                    | D.Jul => "Jul" | D.Aug => "Aug" | D.Sep => "Sep"
                    | D.Oct => "Oct" | D.Nov => "Nov" | D.Dec => "Dec"

  fun format date = let
        fun lz2 num = let val str = Int.toString num
                       in case size str of 1 => "0" ^ str | _ => str end
      in
        String.concat [
          format_wd (D.weekDay date), ", ", lz2 (D.day date), " ",
          format_mon (D.month date), " ", Int.toString (D.year date), " ",
          lz2 (D.hour date), ":", lz2 (D.minute date), ":",
          lz2 (D.second date), " GMT" ]                      
      end                      

end

