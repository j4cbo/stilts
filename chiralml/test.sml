structure R = Reactor(structure T = ThreadBase
                      structure RC = SelectReactorCore)

fun blarg str () = (print str; R.sleep (Time.fromMilliseconds 1000); blarg str ())
val t = R.new (blarg "Mud\n")
val t = R.new (fn () => (R.sleep (Time.fromMilliseconds 500); blarg "Kip\n" ()))
val _ = R.run ()

