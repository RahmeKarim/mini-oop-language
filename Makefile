# Define the compiler to use
OCAMLC=ocamlc
OCAMLLEX=ocamllex
MENHIR=menhir

# Define the source files
LEXER_SRC=lexer.mll
PARSER_SRC=parser.mly
MAIN_SRC=main.ml

# Define the generated files
LEXER_GEN=lexer.ml
PARSER_GEN=parser.ml
PARSER_GEN_INTF=parser.mli

# Define the executable name
EXEC=mini_oop

# Phony targets are not files
.PHONY: all clean

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

# Clean the build directory
clean:
	rm -f *.cmi *.cmo $(LEXER_GEN) $(PARSER_GEN) $(PARSER_GEN_INTF) $(EXEC)
