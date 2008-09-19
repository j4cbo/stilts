SMELT_DEPS = $(shell $(MLTON) -stop f $(SMELT_PATH)/smelt.mlb)

%.grm.sig %.grm.sml: %.grm
	mlyacc $<

%.lex.sml: %.lex
	mllex $<

$(SMELT_PATH)/smelt: $(SMELT_DEPS)
	$(MLTON) $(SMELT_PATH)/smelt.mlb

%.html.sml: %.html $(SMELT_PATH)/smelt
	$(SMELT_PATH)/smelt $<
