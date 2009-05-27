MLB = index.mlb

OUTPUT = index

MLTON = mlton

SQUALL_PATH = ../../../stilts/db/squall

# ---

DEPS = $(shell $(MLTON) -stop f $(MLB))

default: $(OUTPUT)

include $(SQUALL_PATH)/squall.mk

$(OUTPUT): $(DEPS) $(MLB)
	mlton -link-opt -lmysqlclient $(MLB)

$(OUTPUT)-arm: $(DEPS) $(MLB)
	mlton -target arm-poky-linux-gnueabi -cc ~/bin2/arm-poky-linux-gnueabi-gcc -link-opt -lsqlite3 -verbose 2 -output $@ $(MLB)
	~/bin/arm-poky-linux-gnueabi-strip $@
