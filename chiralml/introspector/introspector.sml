functor Introspector(
  structure T: THREAD
) :> sig
  val app : Web.app
end =
struct

  fun app req = let
    val threads = T.get_threads ()

    fun map_thread thr = (
      T.get_id thr,
      T.get_state thr
    )
  in
    WebUtil.htmlResp (
      IntrospectorThreadList.render { threads = map map_thread threads }
    )
  end

end
