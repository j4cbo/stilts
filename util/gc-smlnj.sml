structure GC = struct

  fun collectAll () = SMLofNJ.Internals.GC.doGC 3

end
