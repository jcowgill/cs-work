# CCCP Makefile
#  James Cowgill
#
# This makefile compiles all .c files in src/ into separate executables in build/
#

# Directories
BUILD_DIR   := build
SRC_DIR     := src

# Global Compiler Options
CFLAGS      := -I$(SRC_DIR) -Wall -Wextra -g -std=c99 -pedantic -fno-common
LDFLAGS     := -Wl,--as-needed -lm

#############

# Get list of C files
SOURCES     := $(wildcard $(SRC_DIR)/*.c)

# Get list of executables
TARGETS     := $(addprefix $(BUILD_DIR)/, $(basename $(notdir $(SOURCES))))

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
