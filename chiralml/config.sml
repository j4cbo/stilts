structure ConfigPrintEverything :> THREAD_CONFIG = struct
  datatype tracetype = REACTOR | THREAD | SCHEDULE | ERROR
  fun trace REACTOR f = print ("Reactor: " ^ f () ^ "\n")
    | trace THREAD f = print ("Thread: " ^ f () ^ "\n")
    | trace SCHEDULE f = print ("Schedule: " ^ f () ^ "\n")
    | trace ERROR f = print ("Error: " ^ f () ^ "\n")
end

structure ConfigNoPrint :> THREAD_CONFIG = struct
  datatype tracetype = REACTOR | THREAD | SCHEDULE | ERROR
  fun trace _ _ = ()
end
