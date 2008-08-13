default: FFI-mlton/ffi.mlb

FFI-mlton/ffi.mlb: mysql.h
	mlnlffigen -include ../library.sml -dir FFI-mlton -mlbfile ffi.mlb $^

FFI-smlnj/ffi.cm: mysql.h
	ml-nlffigen -include ../library.sml -dir FFI-smlnj -cmfile ffi.cm $^

clean:
	rm -rf FFI-mlton FFI-smlnj .cm libmysqlclient
