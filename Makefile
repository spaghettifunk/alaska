# Project Settings
debug ?= 0
NAME := alaska
BUILD_DIR := build
BIN_DIR := bin
SRC_DIR := compiler/src
INCLUDE_DIR := compiler/include
LIB_DIR := compiler/lib
TESTS_DIR := compiler/tests

DEFINES := -DDEBUG

# Generate paths for all object files
OBJS := $(patsubst %.c,%.o, $(wildcard $(SRC_DIR)/*.c) $(wildcard $(LIB_DIR)/**/*.c))

# Compiler settings
CC := clang
# LINTER := clang-tidy-18
# FORMATTER := clang-format-18

CFLAGS := -g -MD -Wall -Werror -Wvla -Wgnu-folding-constant -Wno-missing-braces -fdeclspec -fPIC
LDFLAGS := -lm

ifeq ($(debug), 1)
	CFLAGS := $(CFLAGS) -g -O0 $(DEFINES)
else
	CFLAGS := $(CFLAGS) -Oz
endif

# Targets

# Build executable
$(NAME): dir $(OBJS)
	$(CC) $(CFLAGS) $(LDFLAGS) -o $(BIN_DIR)/$@ $(patsubst %, build/%, $(OBJS))

# Build object files and third-party libraries
.PHONY: dir
$(OBJS): dir
	@mkdir -p $(BUILD_DIR)/$(@D)
	@$(CC) $(CFLAGS) -o $(BUILD_DIR)/$@ -c $*.c

.PHONY: run
run: $(NAME) # run the program
	@echo Running...
	@./$(BIN_DIR)/$(NAME) $(INPUT_FILE)