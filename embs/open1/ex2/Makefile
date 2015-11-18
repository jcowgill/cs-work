# Assembly name
ASSEMBLY = MoteSourceNode-1.0

# Source directories
SRC_DIRS = src ../common/src

# Flags to pass to mrc
MRCFLAGS = --verbose -r:logger-11.0

# mrc executable
ifneq (,$(findstring MINGW,$(shell uname -s)))
	MRC = C:/apps/moterunner/win32/bin/mrc.exe
else
	MRC = /usr/share/moterunner/moterunner/linux64/bin/mrc
endif

############

SRCS = $(shell find $(SRC_DIRS) -type f -name '*.java' ! -path '*/test/*')

all: $(ASSEMBLY).sdx

$(ASSEMBLY).sdx: $(SRCS)
	$(MRC) --assembly=$(basename $@) $(MRCFLAGS) $^

# Clean build files
clean:
	rm -f $(ASSEMBLY).sba $(ASSEMBLY).sxp $(ASSEMBLY).sba