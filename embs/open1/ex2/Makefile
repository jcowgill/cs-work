# Flags to pass to mrc
MRCFLAGS = --verbose -r:logger-11.0

# mrc executable
ifneq (,$(findstring MINGW,$(shell uname -s)))
	MRC = C:/apps/moterunner/win32/bin/mrc.exe
else
	MRC = /usr/share/moterunner/moterunner/linux64/bin/mrc
endif

all: MoteSourceNode-1.0.sdx Sink11-1.0.sdx Sink12-1.0.sdx Sink13-1.0.sdx

MoteSourceNode-1.0.sdx: $(shell find src/jcowgill ../common/src -type f -name '*.java' ! -path '*/test/*')
Sink11-1.0.sdx: src/embs/Sink.java src/embs/Sink11.java
Sink12-1.0.sdx: src/embs/Sink.java src/embs/Sink12.java
Sink13-1.0.sdx: src/embs/Sink.java src/embs/Sink13.java

%.sdx:
	$(MRC) --assembly=$(basename $@) $(MRCFLAGS) $^

# Clean build files
clean:
	rm -f *.sba *.sdx *.sxp
