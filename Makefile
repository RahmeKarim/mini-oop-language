# Define the compiler
OCAMLC=ocamlc
OCAMLLEX=ocamllex
MENHIR=menhir
OCAMLFIND=ocamlfind

# Define the source files
LEXER_SRC=lexer.mll
PARSER_SRC=parser.mly
MAIN_SRC=main.ml
TEST_SRC=./tests/test_parser.ml  # Name of test suite file

# Define the generated files
LEXER_GEN=lexer.ml
PARSER_GEN=parser.ml
PARSER_GEN_INTF=parser.mli

# Define the executable name
EXEC=mini_oop
TEST_EXEC=test_parser.native  # Name of the test suite executable

# Phony targets are not files
.PHONY: all clean test

# Default target
all: $(EXEC)

# Rule to make the parser
$(PARSER_GEN) $(PARSER_GEN_INTF): $(PARSER_SRC)
	$(MENHIR) --infer $(PARSER_SRC)

# Rule to make the lexer, depends on parser interface
$(LEXER_GEN): $(LEXER_SRC) $(PARSER_GEN_INTF)
	$(OCAMLLEX) $(LEXER_SRC)

# Rule to make the executable, depends on all generated and source files
$(EXEC): $(LEXER_GEN) $(PARSER_GEN) $(MAIN_SRC)
	$(OCAMLC) -o $(EXEC) $(PARSER_GEN_INTF) $(PARSER_GEN) $(LEXER_GEN) $(MAIN_SRC)

# Rule to run tests
test: $(TEST_SRC) $(LEXER_GEN) $(PARSER_GEN)
	$(OCAMLFIND) ocamlc -o $(TEST_EXEC) -package ounit2 -linkpkg -g $(LEXER_GEN) $(PARSER_GEN) $(PARSER_GEN_INTF) $(TEST_SRC)
	./$(TEST_EXEC)

# Run executable
run: $(EXEC)
	./$(EXEC)

# Clean the build directory
clean:
	rm -f *.cmi *.cmo $(LEXER_GEN) $(PARSER_GEN) $(PARSER_GEN_INTF) $(EXEC) $(TEST_EXEC)
