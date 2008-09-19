MLB = music.mlb
OUTPUT = music

MLTON = mlton

SQUALL_PATH = ../../stilts/db/squall
SMELT_PATH = ../../stilts/smelt

# ---

DEPS = $(shell $(MLTON) -stop f $(MLB))

default: $(OUTPUT)

include $(SQUALL_PATH)/squall.mk
include $(SMELT_PATH)/smelt.mk

$(OUTPUT): $(DEPS) $(MLB)
	mlton -link-opt -ldl $(MLB)
