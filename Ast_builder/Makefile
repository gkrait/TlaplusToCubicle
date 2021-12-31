
############################################################
##     Title: XYZ expressions
##     Author: Jan Midtgaard
##     Date: 2014
##      Availability: https://github.com/jmid/xyz-exp-ast/blob/master/Makefile
############################################################
## Makefile for the Tla2Cubicle grammar
## ---------------------------------------------------------
##
## Usage:
##   make [target]
##
## Targets:
##   Tla2Cubicle.byte     -- build the example as ocaml byte code (default target)
##   Tla2Cubicle.native   -- build the example as native
##   Tla2Cubicle.top      -- build an interactive toplevel
##   Tla2Cubicle.debug    -- build the example with debugging symbols
##   Tla2Cubicle.profile  -- build the example with profiling instrumentation
##   htdoc          -- generate documentation (in doc/index.html)
##   clean          -- clean up all build related files
##
############################################################

# (just ignore this thing...)
.PHONY: default all \
        Tla2Cubicle.byte Tla2Cubicle.native Tla2Cubicle.top Tla2Cubicle.debug Tla2Cubicle.profile \
        htdoc clean prepare findlib

# Default build target (defined below)
# should probably be either 'Tla2Cubicle.native' or 'Tla2Cubicle.byte'
Tla2Cubicle: Tla2Cubicle.byte

# Default target for Eclipse
all: Tla2Cubicle

# Non-source directories (comma separated, no spaces)
# Add directories that do not contain ml source files.
# Fx directories with test cases (will speed up compilation)
EXCL_DIRS := doc

# dOvs modules for generating a toplevel and documentation
# (ie, with source code in src/<module>.ml)
MODS := cst \
        parser lexer \
        main

############################################################
## YOU SHOULD NOT NEED TO EDIT ANYTHING BELOW THIS LINE
############################################################

# Directories
SRC_DIR := .
DOC_DIR := doc
BLD_DIR := _build

# Base invokation of ocamlbuild
OCAMLBUILD := ocamlbuild  -no-links -use-menhir -menhir "menhir -v  " -Xs $(EXCL_DIRS)

Tla2Cubicle.byte: prepare
	@echo "*** Building Tla2Cubicle.byte"
	@$(OCAMLBUILD) $(SRC_DIR)/main.byte
	@ln -sf $(BLD_DIR)/$(SRC_DIR)/main.byte Tla2Cubicle.byte
	@ln -sf Tla2Cubicle.byte Tla2Cubicle

Tla2Cubicle.native: prepare
	@echo "*** Building Tla2Cubicle.native"
	@$(OCAMLBUILD) $(SRC_DIR)/main.native
	@ln -sf $(BLD_DIR)/$(SRC_DIR)/main.native Tla2Cubicle.native
	@ln -sf Tla2Cubicle.native Tla2Cubicle

Tla2Cubicle.debug: prepare
	@echo "*** Building Tla2Cubicle.debug"
	@$(OCAMLBUILD) $(SRC_DIR)/main.d.byte
	@ln -sf _build/$(SRC_DIR)/main.d.byte Tla2Cubicle.debug

Tla2Cubicle.profile: prepare
	@echo "*** Building Tla2Cubicle.profile"
	@$(OCAMLBUILD) $(SRC_DIR)/main.p.native
	@ln -sf _build/$(SRC_DIR)/main.p.native Tla2Cubicle.profile

Tla2Cubicle.top: prepare
	@echo "*** Building Tla2Cubicle.top"
	@echo $(MODS:%=$(SRC_DIR)/%) > $(SRC_DIR)/main.mltop
	@$(OCAMLBUILD) $(SRC_DIR)/main.top
	@rm -f $(SRC_DIR)/main.mltop
	@ln -sf _build/$(SRC_DIR)/main.top Tla2Cubicle.top

htdoc: Tla2Cubicle.byte
	@echo "*** Building $(DOC_DIR)"
	@mkdir -p $(DOC_DIR)
	@ocamldoc -d $(DOC_DIR) -I $(BLD_DIR)/$(SRC_DIR) \
	  -html $(MODS:%=$(BLD_DIR)/$(SRC_DIR)/%.ml)
	@echo "Open $(DOC_DIR)/index.html to browse the documentation."

clean:
	$(OCAMLBUILD) -clean
	rm -rf $(DOC_DIR)
	rm -f Tla2Cubicle Tla2Cubicle.byte Tla2Cubicle.native Tla2Cubicle.top Tla2Cubicle.debug Tla2Cubicle.profile
	rm -f $(SRC_DIR)/main.mltop
	rm -f .ocamlinit
	rm -f parser.automaton

## Some specially generated files
.ocamlinit:
	@echo "#directory \"$(BLD_DIR)/$(SRC_DIR)\";;" > .ocamlinit

parser.automaton:
	@ln -sf $(BLD_DIR)/$(SRC_DIR)/parser.automaton parser.automaton

prepare: .ocamlinit parser.automaton
