MLB = music.mlb
OUTPUT = sing

MLTON = mlton

SQUALL_PATH = ../../stilts/db/squall
MYSQL_PATH = ../../stilts/db/libmysqlclient
SMELT_PATH = ../../stilts/smelt

DATE=$(shell date +%Y%m%d)

# ---

all: $(MYSQL_PATH)/FFI-mlton/ffi.mlb $(OUTPUT) index/index

$(MYSQL_PATH)/FFI-mlton/ffi.mlb:
	make -C $(MYSQL_PATH) FFI-mlton/ffi.mlb

index/index:
	make -C index index

DEPS = $(shell $(MLTON) -stop f $(MLB))

default: $(OUTPUT)

include $(SQUALL_PATH)/squall.mk
include $(SMELT_PATH)/smelt.mk

$(OUTPUT): $(MYSQL_PATH)/FFI-mlton/ffi.mlb $(DEPS) $(MLB)
	mlton -link-opt -lmysqlclient -output $@ $(MLB)

$(OUTPUT)-arm: $(DEPS) $(MLB)
	mlton -target arm-poky-linux-gnueabi -cc ~/bin/arm-poky-linux-gnueabi-gcc -output $@ $(MLB)
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
