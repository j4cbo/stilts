SQUALL_DEPS = $(shell $(MLTON) -stop f $(SQUALL_PATH)/squall.mlb)

%.grm.sig %.grm.sml: %.grm
	mlyacc $<

%.lex.sml: %.lex
	mllex $<

$(SQUALL_PATH)/squall: $(SQUALL_DEPS)
	$(MLTON) $(SQUALL_PATH)/squall.mlb

%.squall.sml: %.squall $(SQUALL_PATH)/squall
	$(SQUALL_PATH)/squall $<
