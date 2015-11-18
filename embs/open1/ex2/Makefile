# Flags to pass to mrc
MRCFLAGS = --verbose -r:logger-11.0

# mrc executable
ifneq (,$(findstring MINGW,$(shell uname -s)))
	MRC = C:/apps/moterunner/win32/bin/mrc.exe
else
	MRC = /usr/share/moterunner/moterunner/linux64/bin/mrc
endif

all: MoteSourceNode-1.0.sdx Sink-1.0.sdx

MoteSourceNode-1.0.sdx: $(shell find src/jcowgill ../common/src -type f -name '*.java' ! -path '*/test/*')
Sink-1.0.sdx: $(shell find src/embs -type f -name '*.java')

%.sdx:
	$(MRC) --assembly=$(basename $@) $(MRCFLAGS) $^

# Clean build files
clean:
	rm -f *.sba *.sdx *.sxp
