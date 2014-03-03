# Makefile for SYAC Compilers
#  Extra files (alex, happy and gitignores)
#

SRC_ALEX  := $(shell find * -type f -name '*.x')
SRC_HAPPY := $(shell find * -type f -name '*.y')

TARGET_ALEX  := $(SRC_ALEX:.x=.hs)
TARGET_HAPPY := $(SRC_HAPPY:.y=.hs)

ALEX := alex
HAPPY := happy

ALEX_FLAGS := -g
HAPPY_FLAGS := -g

############################

all: $(TARGET_ALEX) $(TARGET_HAPPY) .gitignore

clean:
	rm -f $(TARGET_ALEX) $(TARGET_HAPPY)

.gitignore: $(SRC_ALEX) $(SRC_HAPPY)
	echo "# AUTOMATICALLY GENERATED" > $@
	echo $(TARGET_ALEX) $(TARGET_HAPPY) | tr ' ' '\n' >> $@

%.hs: %.x
	$(ALEX) $(ALEX_FLAGS) -o $@ $<

%.hs: %.y
	$(HAPPY) $(HAPPY_FLAGS) -o $@ $<
