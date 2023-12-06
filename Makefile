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
STATIC_ANALYZER_SRC=static_analyzer.ml
SEMANTIC_DOMAIN_SRC=semantic_domain.ml
OPERATIONAL_SEMANTICS_SRC=operational_semantics.ml
MAIN_SRC=main.ml

TEST_LEXER_SRC=./tests/test_lexer.ml
TEST_PARSER_SRC=./tests/test_parser.ml
TEST_PRETTY_PRINTER_SRC=./tests/test_pretty_printer.ml
TEST_STATIC_ANALYZER_SRC=./tests/test_static_analyzer.ml
TEST_OPERATIONAL_SEMANTICS_SRC=./tests/test_operational_semantics.ml

# Define the generated files
LEXER_GEN=lexer.ml
PARSER_GEN=parser.ml
PARSER_GEN_INTF=parser.mli

# Define the executable names
EXEC=mini_oop
TEST_LEXER=test_lexer.native
TEST_PARSER=test_parser.native
TEST_PRETTY_PRINTER=test_pretty_printer.native
TEST_STATIC_ANALYZER=test_static_analyzer.native
TEST_OPERATIONAL_SEMANTICS=test_operational_semantics.native

# Phony targets are not files
.PHONY: all clean test verbose test_pretty_printer

# Default target
all: $(EXEC)

# Rule to compile Static Analyzer
$(STATIC_ANALYZER_SRC:.ml=.cmo): $(STATIC_ANALYZER_SRC)
	$(OCAMLC) -c $(STATIC_ANALYZER_SRC)

# Rule to compile AST
$(AST_SRC:.ml=.cmo): $(AST_SRC)
	$(OCAMLC) -c $(AST_SRC)

# Rule to compile Semantic Domain
$(SEMANTIC_DOMAIN_SRC:.ml=.cmo): $(SEMANTIC_DOMAIN_SRC)
	$(OCAMLC) -c $(SEMANTIC_DOMAIN_SRC)

# Rule to compile Pretty Printer
$(PRETTY_PRINTER_SRC:.ml=.cmo): $(PRETTY_PRINTER_SRC)
	$(OCAMLC) -c $(PRETTY_PRINTER_SRC)

# Rule to make the parser
$(PARSER_GEN) $(PARSER_GEN_INTF): $(PARSER_SRC) $(AST_SRC:.ml=.cmo)
	$(MENHIR) --infer $(PARSER_SRC)
	$(OCAMLC) -c $(PARSER_GEN_INTF) $(PARSER_GEN)

# Rule to compile Operational Semantics
$(OPERATIONAL_SEMANTICS_SRC:.ml=.cmo): $(OPERATIONAL_SEMANTICS_SRC) $(AST_SRC:.ml=.cmo) $(PARSER_GEN)
	$(OCAMLC) -c $(OPERATIONAL_SEMANTICS_SRC)

# Rule to make the lexer, depends on parser interface
$(LEXER_GEN): $(LEXER_SRC) $(PARSER_GEN_INTF)
	$(OCAMLLEX) $(LEXER_SRC)
	$(OCAMLC) -c $(LEXER_GEN)

# Rule to make the executable, depends on all generated and source files
$(EXEC): $(LEXER_GEN) $(PARSER_GEN) $(AST_SRC:.ml=.cmo) $(PRETTY_PRINTER_SRC:.ml=.cmo) $(STATIC_ANALYZER_SRC:.ml=.cmo) $(SEMANTIC_DOMAIN_SRC:.ml=.cmo) $(OPERATIONAL_SEMANTICS_SRC:.ml=.cmo) $(MAIN_SRC)
	$(OCAMLC) -o $(EXEC) $(PARSER_GEN_INTF) $(PARSER_GEN) $(LEXER_GEN) $(AST_SRC:.ml=.cmo) $(PRETTY_PRINTER_SRC:.ml=.cmo) $(STATIC_ANALYZER_SRC:.ml=.cmo) $(SEMANTIC_DOMAIN_SRC:.ml=.cmo) $(OPERATIONAL_SEMANTICS_SRC:.ml=.cmo) $(MAIN_SRC)

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
	$(OCAMLFIND) ocamlc -o $(TEST_PRETTY_PRINTER) -package ounit2 -linkpkg $(AST_SRC:.ml=.cmo) $(PRETTY_PRINTER_SRC:.ml=.cmo) $(LEXER_GEN:.mll=.cmo) $(PARSER_GEN:.mly=.cmo) $(TEST_PRETTY_PRINTER_SRC)
	./$(TEST_PRETTY_PRINTER)

# Rule to compile and run the static analyzer test
test_static_analyzer: $(PRETTY_PRINTER_SRC:.ml=.cmo) $(STATIC_ANALYZER_SRC:.ml=.cmo) $(AST_SRC:.ml=.cmo) $(LEXER_GEN:.mll=.cmo) $(PARSER_GEN:.mly=.cmo) $(TEST_STATIC_ANALYZER_SRC)
	$(OCAMLFIND) ocamlc -o $(TEST_STATIC_ANALYZER) -package ounit2 -linkpkg $(AST_SRC:.ml=.cmo) $(PRETTY_PRINTER_SRC:.ml=.cmo) $(STATIC_ANALYZER_SRC:.ml=.cmo) $(LEXER_GEN:.mll=.cmo) $(PARSER_GEN:.mly=.cmo) $(TEST_STATIC_ANALYZER_SRC)
	./$(TEST_STATIC_ANALYZER)

# Rule to compile and run the operational semantics test
test_operational_semantics: $(PRETTY_PRINTER_SRC:.ml=.cmo) $(STATIC_ANALYZER_SRC:.ml=.cmo) $(SEMANTIC_DOMAIN_SRC:.ml=.cmo) $(OPERATIONAL_SEMANTICS_SRC:.ml=.cmo) $(AST_SRC:.ml=.cmo) $(LEXER_GEN:.mll=.cmo) $(PARSER_GEN:.mly=.cmo) $(TEST_OPERATIONAL_SEMANTICS_SRC)
	$(OCAMLFIND) ocamlc -o $(TEST_OPERATIONAL_SEMANTICS) -package ounit2 -linkpkg $(PRETTY_PRINTER_SRC:.ml=.cmo) $(STATIC_ANALYZER_SRC:.ml=.cmo) $(SEMANTIC_DOMAIN_SRC:.ml=.cmo) $(OPERATIONAL_SEMANTICS_SRC:.ml=.cmo) $(AST_SRC:.ml=.cmo) $(LEXER_GEN:.mll=.cmo) $(PARSER_GEN:.mly=.cmo) $(TEST_OPERATIONAL_SEMANTICS_SRC)
	./$(TEST_OPERATIONAL_SEMANTICS)

# Run all tests
test: test_lexer test_parser test_pretty_printer test_static_analyzer test_operational_semantics

# Run executable
run: $(EXEC)
	./$(EXEC)

# Clean the build directory and oUnit test artifacts
clean:
	rm -f  ./tests/*.cmi ./tests/*.cmo *.cmi *.cmo oUnit-Test* parser.conflicts $(LEXER_GEN) $(PARSER_GEN) $(PARSER_GEN_INTF) $(EXEC) $(TEST_LEXER) $(TEST_PARSER) $(TEST_PRETTY_PRINTER) $(TEST_STATIC_ANALYZER) $(TEST_OPERATIONAL_SEMANTICS)