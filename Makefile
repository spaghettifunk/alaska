BUILD_DIR := bin
OBJ_DIR := obj

ASSEMBLY := compiler
EXTENSION := .a
COMPILER_FLAGS := -g -MD -Wall -Werror -Wvla -Wgnu-folding-constant -Wno-missing-braces -fdeclspec -fPIC
INCLUDE_FLAGS := -Icompiler/src
LINKER_FLAGS := -g -shared

DEFINES := -D_DEBUG -DKEXPORT

# Make does not offer a recursive wildcard function, so here's one:
#rwildcard=$(wildcard $1$2) $(foreach d,$(wildcard $1*),$(call rwildcard,$d/,$2))

SRC_FILES := $(shell find $(ASSEMBLY) -type f \( -name "*.c" \))	# .c files
DIRECTORIES := $(shell find $(ASSEMBLY) -type d)	# directories with .h files
OBJ_FILES := $(SRC_FILES:%=$(OBJ_DIR)/%.o)

all: scaffold compile link

.PHONY: scaffold
scaffold: # create build directory
	@echo Scaffolding folder structure...
	@mkdir -p $(addprefix $(OBJ_DIR)/,$(DIRECTORIES))
	@echo Done.

.PHONY: link
link: scaffold $(OBJ_FILES) # link
	@echo Linking $(ASSEMBLY)...
	@mkdir -p $(BUILD_DIR)
	@clang $(OBJ_FILES) -o $(BUILD_DIR)/lib$(ASSEMBLY)$(EXTENSION) $(LINKER_FLAGS)

.PHONY: compile
compile: #compile .c
	@echo Compiling...
-include $(OBJ_FILES:.o=.d)
.PHONY: clean
clean: # clean build directory
	rm -Rf $(BUILD_DIR)\lib$(ASSEMBLY)$(EXTENSION)
	rm -Rf $(OBJ_DIR)\$(ASSEMBLY)

$(OBJ_DIR)/%.c.o: %.c # compile .c to .o object
	@echo   $<...
	@clang $< $(COMPILER_FLAGS) -c -o $@ $(DEFINES) $(INCLUDE_FLAGS)

-include $(OBJ_FILES:.o=.d)

.PHONY: run
run: all # run the program
	@echo Running...
	@./$(BUILD_DIR)/lib$(ASSEMBLY)$(EXTENSION) $(INPUT_FILE)