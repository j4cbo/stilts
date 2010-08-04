structure Curl :> CURL = struct

	type ptr = MLton.Pointer.t

	fun read_bytes (p, n) =
		Word8Vector.tabulate (n, fn i => MLton.Pointer.getWord8 (p, i))

	fun curl url =
	let
		val p =
			(_import "curl_supereasy" : string -> ptr;)
			(url)

		val len =
			(_import "curl_supereasy_len" : ptr -> int;)
			(p)

		val data =
			(_import "curl_supereasy_data" : ptr -> ptr;)
			(p)

		val ret = Byte.bytesToString (read_bytes (data, len))

		val _ =
			(_import "curl_supereasy_cleanup" : ptr -> unit;)
			(p)
	in
		ret
	end

end
