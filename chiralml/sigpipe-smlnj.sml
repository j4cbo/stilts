structure SIGPIPE = struct
  fun ignore () = (Signals.setHandler (UnixSignals.sigPIPE, Signals.IGNORE);
                   ())
end
