# Compile all ada programs into out

GNATMAKE := gnatmake
ADAFLAGS := -D /tmp -q
OUTDIR   := out

SOURCES  := $(wildcard *.adb)
TARGETS  := $(addprefix $(OUTDIR)/, $(basename $(SOURCES)))

###########
all: $(TARGETS)

clean:
	rm -rf $(OUTDIR)

$(OUTDIR)/%: %.adb
	@echo "  [GNATMAKE] $<"
	@mkdir -p $(dir $@)
	@$(GNATMAKE) $(ADAFLAGS) -o $@ $<
