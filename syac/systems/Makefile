# SYAC Makefile
#  James Cowgill
#
#  Based off makefile used I used in the cccp module
#

# Directories
BUILD_DIR   := build
SRC_DIR     := src

# Source subdirectories
SRC_SUB_DIRS := .

# Global Compiler Options
CFLAGS      := -I$(SRC_DIR) -Wall -Wextra -g -std=c99 -pedantic -fno-common -D_POSIX_C_SOURCE=200112L
LDFLAGS     := -Wl,--as-needed -lm -lpthread

#############

# Get list of C files
SRC_DIRS    := $(addprefix $(SRC_DIR)/, $(SRC_SUB_DIRS))
SOURCES     := $(wildcard $(addsuffix /*.c, $(SRC_DIRS)))

# Get list of executables
TARGETS     := $(basename $(subst $(SRC_DIR)/, $(BUILD_DIR)/, $(SOURCES)))

# Special rules
all: $(TARGETS)

clean:
	rm -rf $(BUILD_DIR)

.SUFFIXES:
.PHONY: all clean

# Main build rule
$(BUILD_DIR)/%: $(SRC_DIR)/%.c
	@echo "  [CC] $<"
	@mkdir -p $(dir $@)
	@$(CC) $(CFLAGS) -o $@ $< $(LDFLAGS)
