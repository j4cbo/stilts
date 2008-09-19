MLB = music.mlb
OUTPUT = music

MLTON = mlton

SQUALL_PATH = ../../stilts/db/squall
MYSQL_PATH = ../../stilts/db/libmysqlclient
SMELT_PATH = ../../stilts/smelt

DATE=$(shell date +%Y%m%d)

# ---

all: $(MYSQL_PATH)/FFI-mlton/ffi.mlb music index/index

$(MYSQL_PATH)/FFI-mlton/ffi.mlb:
	make -C $(MYSQL_PATH) FFI-mlton/ffi.mlb

index/index:
	make -C index index

DEPS = $(shell $(MLTON) -stop f $(MLB))

default: $(OUTPUT)

include $(SQUALL_PATH)/squall.mk
include $(SMELT_PATH)/smelt.mk

$(OUTPUT): $(MYSQL_PATH)/FFI-mlton/ffi.mlb $(DEPS) $(MLB)
	mlton -link-opt -ldl $(MLB)

dist: sing-$(DATE)

sing-$(DATE): default
	rm -rf $@
	mkdir $@
	cp -a index/index mysql-mods.sql lighttpd.conf $@/
	cp music $@/sing
	strip $@/index
	strip $@/sing
	svn export static $@/static
	tar czf $@.tar.gz $@
	du -hs $@ $@.tar.gz
