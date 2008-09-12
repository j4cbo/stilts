structure Cache :> sig

	type 'a cached

	val mk: (unit -> 'a) -> 'a cached

	val get: 'a cached -> 'a

	val resetAll: unit -> unit

end = struct

	type 'a cached = 'a option ref * (unit -> 'a)

        val resetList : (unit -> unit) list ref = ref nil

	fun mk f = let
                      val cell = ref NONE
                      fun reset () = (cell := NONE)
                    in
                      resetList := (reset :: !resetList);
                      (cell, f)
                    end

	fun get (cell, f) =
              case !cell of SOME value => value
                          | NONE => let
                              val v = f ()
                            in
                              cell := SOME v;
                              v
                            end

        fun resetAll () = (app (fn c => c ()) (!resetList);
                           resetList := nil)

end
