squall: input.sml input.lex input.grm squall-sqlite.sml squall-standalone.sml squall.mlb
	mllex input.lex
	mlyacc input.grm
	mlton squall.mlb

heap:
	ml-build squall.cm Squall.main heap

clean:
	rm -rf input.lex.sml input.grm.sig input.grm.sml squall heap.* .cm
