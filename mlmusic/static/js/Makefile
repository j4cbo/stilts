SRCS = jquery-1.2.6.min.js jquery-ui-personalized-1.5.2.js sing.js

sing-all.js: $(SRCS)
	cat $^ > $@

dist: $(SRCS)
	cat $^ > $@
	rm $^ Makefile
