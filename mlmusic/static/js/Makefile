SRCS = jquery-1.2.6.min.js jquery-ui-personalized-1.5.2.js sing.js

sing-all.js: $(SRCS)
	cat $^ | ./jsmin.py > $@

dist: $(SRCS)
	cat $^ | ./jsmin.py > sing-all.js
	rm $^ jsmin.py Makefile
