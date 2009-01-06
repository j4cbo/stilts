structure SIGPIPE = struct
  fun ignore () = (MLton.Signal.setHandler (Posix.Signal.pipe,
                                            MLton.Signal.Handler.ignore);
                   ())
end
