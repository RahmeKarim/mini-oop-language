# Define the compiler
OCAMLC=ocamlc
OCAMLLEX=ocamllex
MENHIR=menhir
OCAMLFIND=ocamlfind

# Define the source files
LEXER_SRC=lexer.mll
PARSER_SRC=parser.mly
AST_SRC=ast.ml
PRETTY_PRINTER_SRC=pretty_printer.ml
MAIN_SRC=main.ml
TEST_LEXER_SRC=./tests/test_lexer.ml
TEST_PARSER_SRC=./tests/test_parser.ml
TEST_PRETTY_PRINTER_SRC=./tests/test_pretty_printer.ml

# Define the generated files
LEXER_GEN=lexer.ml
PARSER_GEN=parser.ml
PARSER_GEN_INTF=parser.mli

# Define the executable name
EXEC=mini_oop
TEST_LEXER=test_lexer.native
TEST_PARSER=test_parser.native
TEST_PRETTY_PRINTER_EXEC=test_pretty_printer.native

# Phony targets are not files
.PHONY: all clean test verbose test_pretty_printer

# Default target
all: $(EXEC)

# Rule to compile AST
$(AST_SRC:.ml=.cmo): $(AST_SRC)
	$(OCAMLC) -c $(AST_SRC)

# Rule to compile Pretty Printer
$(PRETTY_PRINTER_SRC:.ml=.cmo): $(PRETTY_PRINTER_SRC)
	$(OCAMLC) -c $(PRETTY_PRINTER_SRC)

# Rule to make the parser
$(PARSER_GEN) $(PARSER_GEN_INTF): $(PARSER_SRC) $(AST_SRC:.ml=.cmo)
	$(MENHIR) --infer $(PARSER_SRC)
	$(OCAMLC) -c $(PARSER_GEN_INTF) $(PARSER_GEN)

# Rule to make the lexer, depends on parser interface
$(LEXER_GEN): $(LEXER_SRC) $(PARSER_GEN_INTF)
	$(OCAMLLEX) $(LEXER_SRC)
	$(OCAMLC) -c $(LEXER_GEN)

# Rule to make the executable, depends on all generated and source files
$(EXEC): $(LEXER_GEN) $(PARSER_GEN) $(AST_SRC:.ml=.cmo) $(PRETTY_PRINTER_SRC:.ml=.cmo) $(MAIN_SRC)
	$(OCAMLC) -o $(EXEC) $(PARSER_GEN_INTF) $(PARSER_GEN) $(LEXER_GEN) $(AST_SRC:.ml=.cmo) $(PRETTY_PRINTER_SRC:.ml=.cmo) $(MAIN_SRC)

# Additional target for verbose Menhir
verbose: $(PARSER_SRC)
	$(MENHIR) --explain --infer $(PARSER_SRC)

# Rule to run tests
test_lexer: $(LEXER_GEN:.mll=.cmo) $(PARSER_GEN:.mly=.cmo) $(AST_SRC:.ml=.cmo) $(PRETTY_PRINTER_SRC:.ml=.cmo) $(TEST_LEXER_SRC)
	$(OCAMLFIND) ocamlc -o $(TEST_LEXER) -package ounit2 -linkpkg -g $(LEXER_GEN:.mll=.cmo) $(PARSER_GEN:.mly=.cmo) $(TEST_LEXER_SRC)
	./$(TEST_LEXER)

# Rule to run tests
test_parser: $(LEXER_GEN:.mll=.cmo) $(PARSER_GEN:.mly=.cmo) $(AST_SRC:.ml=.cmo) $(PRETTY_PRINTER_SRC:.ml=.cmo) $(TEST_PARSER_SRC)
	$(OCAMLFIND) ocamlc -o $(TEST_PARSER) -package ounit2 -linkpkg -g $(LEXER_GEN:.mll=.cmo) $(PARSER_GEN:.mly=.cmo) $(AST_SRC:.ml=.cmo) $(PRETTY_PRINTER_SRC:.ml=.cmo) $(TEST_PARSER_SRC)
	./$(TEST_PARSER)


# Rule to compile and run the pretty printer test
test_pretty_printer: $(PRETTY_PRINTER_SRC:.ml=.cmo) $(AST_SRC:.ml=.cmo) $(LEXER_GEN:.mll=.cmo) $(PARSER_GEN:.mly=.cmo) $(TEST_PRETTY_PRINTER_SRC)
	$(OCAMLFIND) ocamlc -o $(TEST_PRETTY_PRINTER_EXEC) -package ounit2 -linkpkg $(AST_SRC:.ml=.cmo) $(PRETTY_PRINTER_SRC:.ml=.cmo) $(LEXER_GEN:.mll=.cmo) $(PARSER_GEN:.mly=.cmo) $(TEST_PRETTY_PRINTER_SRC)
	./$(TEST_PRETTY_PRINTER_EXEC)

# Run executable
run: $(EXEC)
	./$(EXEC)

# Clean the build directory
clean:
	rm -f *.cmi *.cmo $(LEXER_GEN) $(PARSER_GEN) $(PARSER_GEN_INTF) $(EXEC) $(TEST_LEXER) $(TEST_PARSER) $(TEST_PRETTY_PRINTER)