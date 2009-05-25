structure SelectReactorCore :> REACTOR_CORE = struct

  structure CC = ChiralCommon

  type 'a sock_block_info = 'a * CC.block_cond * Socket.sock_desc
  type 'a state = 'a sock_block_info list ref

  fun init () = ref nil

  fun add_sock state (e, cond, desc) = 
        state := (e, cond, desc) :: !state

  fun rdwr_fold ((_, CC.BLOCK_RD, sock), (rd, wr)) = (sock :: rd, wr)
    | rdwr_fold ((_, CC.BLOCK_WR, sock), (rd, wr)) = (rd, sock :: wr)

  fun wait (ref nil) NONE = NONE
    | wait state timeout = let

        val bt = !state

        val (rd, wr) = foldl rdwr_fold (nil, nil) bt

        val tstr = case timeout of
                      NONE => "no"
                    | SOME t => IntInf.toString (Time.toMilliseconds t) ^ " ms"

        val () = print ("  select(): waiting on " ^ Int.toString (length rd)
                        ^ " read, " ^ Int.toString (length wr) ^ " write, "
                        ^ tstr ^ " timeout.\n");

        val sparam = { rds = rev rd, wrs = rev wr,
                       exs = nil, timeout = timeout }

        val { rds, wrs, exs } = (Socket.select sparam
                                 handle e => raise e)

        val () = print ("  select(): returned " ^ Int.toString (length rds)
                        ^ " read, " ^ Int.toString (length wrs) ^ " write\n");

        fun res_fold (i as (ent, CC.BLOCK_RD, sock), (nil, wrs, out, rem)) =
              (nil, wrs, out, i :: rem)
          | res_fold (i as (ent, CC.BLOCK_RD, sock), (r::rds, wrs, out, rem)) =
              if Socket.sameDesc (sock, r)
              then (rds, wrs, ent::out, rem)
              else (r::rds, wrs, out, i :: rem)
          | res_fold (i as (ent, CC.BLOCK_WR, sock), (rds, nil, out, rem)) =
              (rds, nil, out, i :: rem)
          | res_fold (i as (ent, CC.BLOCK_WR, sock), (rds, w::wrs, out, rem)) =
              if Socket.sameDesc (sock, w)
              then (rds, wrs, ent::out, rem)
              else (rds, w::wrs, out, i :: rem)
        val (urds, uwrs, out, rem) = foldl res_fold (rds, wrs, nil, nil) bt

        val () = case (urds, uwrs) of (nil, nil) => ()
                                    | _ => raise Fail "unmatched select result"

        val () = state := rev rem
      in
        SOME out
      end

end
