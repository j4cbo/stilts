MLB = music.mlb
OUTPUT = music

MLTON = mlton

SQUALL_PATH = ../../stilts/db/squall
MYSQL_PATH = ../../stilts/db/libmysqlclient
SMELT_PATH = ../../stilts/smelt

# ---

default: $(MYSQL_PATH)/FFI-mlton/ffi.mlb music index/index

$(MYSQL_PATH)/FFI-mlton/ffi.mlb:
	make -C $(MYSQL_PATH) FFI-mlton/ffi.mlb

DEPS = $(shell $(MLTON) -stop f $(MLB))

default: $(OUTPUT)

include $(SQUALL_PATH)/squall.mk
include $(SMELT_PATH)/smelt.mk

$(OUTPUT): $(MYSQL_PATH)/FFI-mlton/ffi.mlb $(DEPS) $(MLB)
	mlton -link-opt -ldl $(MLB)
