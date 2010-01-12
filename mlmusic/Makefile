MLB = music.mlb
OUTPUT = sing

MLTON = mlton

SQUALL_PATH = ../../stilts/db/squall
MYSQL_PATH = ../../stilts/db/libmysqlclient
SMELT_PATH = ../../stilts/smelt

DATE=$(shell date +%Y%m%d)

# ---

all: $(OUTPUT)

DEPS = $(shell $(MLTON) -stop f $(MLB))

default: $(OUTPUT)

include $(SQUALL_PATH)/squall.mk
include $(SMELT_PATH)/smelt.mk

$(OUTPUT): $(MYSQL_PATH)/FFI-mlton/ffi.mlb $(DEPS) $(MLB)
	mlton -link-opt -lmysqlclient -output $@ $(MLB)

$(OUTPUT)-arm: $(DEPS) $(MLB)
	./mlton-static-libgmp -target arm-poky-linux-gnueabi -cc ~/bin2/arm-poky-linux-gnueabi-gcc -link-opt -lsqlite3 -verbose 2 -output $@ $(MLB)
	~/bin/arm-poky-linux-gnueabi-strip $@

dist: sing-$(DATE)

sing-$(DATE): all
	rm -rf $@
	mkdir $@
	cp -a sing index/index mysql-mods.sql lighttpd.conf $@/
	strip $@/index
	strip $@/sing
	svn export static $@/static
	tar czf $@.tar.gz $@
	du -hs $@ $@.tar.gz

dist-arm: sing-arm-$(DATE)

sing-arm-$(DATE): sing-arm
	rm -rf $@
	mkdir $@
	cp -a sing-arm $@/
	svn export static $@/static
	make -C $@/static/js dist
	tar czf $@.tar.gz $@
	du -hs $@ $@.tar.gz
