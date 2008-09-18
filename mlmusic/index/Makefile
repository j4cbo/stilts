MLB = index.mlb
OUTPUT = index

MLTON = mlton

SQUALL_PATH = ../../../stilts/db/squall

# ---

DEPS = $(shell $(MLTON) -stop f $(MLB))

default: $(OUTPUT)

include $(SQUALL_PATH)/squall.mk

$(OUTPUT): $(DEPS) $(MLB)
	mlton -link-opt -ldl $(MLB)
