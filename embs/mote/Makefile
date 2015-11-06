# Assemblies to build
#  Source directories are everything before the dash
ASSEMBLY = LED-1.0 RadioReceiver-1.0 RadioSender-1.0

# Flags to pass to mrc
MRCFLAGS = --verbose -r:logger-11.0

# mrc executable
ifneq (,$(findstring MINGW,$(shell uname -s)))
	MRC = C:/apps/moterunner/win32/bin/mrc.exe
else
	MRC = /usr/share/moterunner/moterunner/linux64/bin/mrc
endif

############

SRCS = $(shell find $$(echo $(1) | sed 's/-.*//') -type f -name '*.java')

all: $(addsuffix .sdx, $(ASSEMBLY))

.SECONDEXPANSION:
%.sdx: $$(call SRCS, $$@)
	$(MRC) --assembly=$(basename $@) $(MRCFLAGS) $^

# Clean build files
clean:
	rm -f $(addsuffix .sba, $(ASSEMBLY))
	rm -f $(addsuffix .sdx, $(ASSEMBLY))

# Clean absolutely all files
distclean: clean
	rm -f $(addsuffix .sxp, $(ASSEMBLY))
