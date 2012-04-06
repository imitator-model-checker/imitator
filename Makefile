###############################################################
#
#                    IMITATOR 2.5
#
#  Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
#
#  Author:        Etienne Andre, Ulrich KUEHNE, Romain SOULAT
#  Created:       2009/09/07
#  Last modified: 2012/02/20
#  Ocaml version: 3.11.2
###############################################################

# flags for ocaml compiler
# OCAMLC_FLAGS = -g
OCAMLC_FLAGS = 

# ocaml compiler
OCAMLC = ocamlc $(OCAMLC_FLAGS)
OCAMLOPT = ocamlopt.opt $(OCAMLC_FLAGS) -compact

# path variables
ifndef EXTLIB_PATH
  EXTLIB_PATH = /usr/lib/ocaml/extlib
endif
ifndef OUNIT_PATH
  OUNIT_PATH = $(HOME)/local/ounit
endif
ifndef OCAML_PPL_PATH
  #OCAML_PPL_PATH = $(HOME)/local/lib/ppl
	OCAML_PPL_PATH = /usr/lib/ppl
endif 
ifndef OCAML_GMP_PATH
 #OCAML_GMP_PATH = $(HOME)/local/lib
	OCAML_GMP_PATH = /usr/lib/ocaml/gmp
endif
ifndef CLIB_PATH
  CLIB_PATH = /usr/lib -I /usr/local/lib
endif 

# export paths for use in sub-makefiles
export EXTLIB_PATH 
export OUNIT_PATH 
export OCAML_PPL_PATH
export OCAML_GMP_PATH
export CLIB_PATH

INCLUDE = -I $(SRC) -I $(EXTLIB_PATH) -I $(OCAML_PPL_PATH) -I $(OCAML_GMP_PATH) -I $(CLIB_PATH)

# external libs for compiling imitator
# export LIBS = str.cma unix.cma extLib.cma bigarray.cma gmp.cma apron.cma polkaMPQ.cma

# native c libraries
# CLIBS = -cclib -lpwl -cclib -lm -cclib -lgmpxx -cclib -lgmp -cclib -lppl

# For 64 bits compiling
CLIBS = -cclib '-static -lppl -lpwl -lppl_ocaml -lstdc++ -lmlgmp -lmpfr -lgmp -lgmpxx ' 

# ALLOWS STATIC COMPILING IN 32 BITS :-)
# CLIBS = -cclib '-static -lppl -lpwl -lcamlrun -ltinfo -lppl_ocaml -lstdc++ -lmlgmp -lmpfr -lgmp -lgmpxx'

# ocaml lib files
OLIBS = str.cma unix.cma extLib.cma bigarray.cma gmp.cma ppl_ocaml.cma 

# native ocaml lib files
OOLIBS = str.cmxa unix.cmxa extLib.cmxa bigarray.cmxa gmp.cmxa ppl_ocaml.cmxa

# external libs for compiling with PPL support
export LIBS = $(CLIBS) $(OLIBS)
export OPTLIBS = $(CLIBS) $(OOLIBS) 


SRC = src

# FILES
.PREFIXES : +.
.SUFFIXES : .cmo .cmi .ml .mli .cmxo

# main object
MAIN = $(SRC)/IMITATOR.cmo
MAIN_OPT = $(MAIN:.cmo=.cmx)

# sources to compile
FILES =  $(SRC)/Global.+ $(SRC)/Cache.+ $(SRC)/Options.+ $(SRC)/NumConst.+  $(SRC)/LinearConstraint.+ $(SRC)/Program.+ $(SRC)/Graphics.+ $(SRC)/Automaton.+ $(SRC)/Pi0Lexer.+ $(SRC)/Pi0Parser.+ $(SRC)/Pi0CubeLexer.+ $(SRC)/Pi0CubeParser.+ $(SRC)/ImitatorLexer.+ $(SRC)/ImitatorParser.+ $(SRC)/ImitatorPrinter.+ $(SRC)/ProgramConverter.+ $(SRC)/ProgramInverter.+ $(SRC)/PredicateAbstraction.+ $(SRC)/Graph.+  $(SRC)/Reachability.+ 
OBJS = $(FILES:+=cmo)
OBJS_OPT = $(OBJS:.cmo=.cmx)

# header files
FILESMLI = $(SRC)/Global.+ $(SRC)/Cache.+ $(SRC)/NumConst.+ $(SRC)/LinearConstraint.+ $(SRC)/Graphics.+ $(SRC)/Automaton.+ $(SRC)/ParsingStructure.+ $(SRC)/AbstractImitatorFile.+ $(SRC)/Program.+ $(SRC)/ImitatorPrinter.+ $(SRC)/ProgramConverter.+ $(SRC)/ProgramInverter.+ $(SRC)/PredicateAbstraction.+ $(SRC)/Graph.+  $(SRC)/Reachability.+  

# parsers and lexers 
LEXERS = $(SRC)/Pi0Lexer.+ $(SRC)/Pi0CubeLexer.+ $(SRC)/ImitatorLexer.+
PARSERS = $(SRC)/Pi0Parser.+ $(SRC)/Pi0CubeParser.+ $(SRC)/ImitatorParser.+

# target library
IMILIB = lib/imitator.cma
IMILIB_OPT = $(IMILIB:.cma=.cmxa)

EXAMPLE_PATH = examples


# target executable
TARGET = bin/IMITATOR
TARGET_OPT = bin/IMITATOR.opt

default all: $(TARGET)
opt: $(TARGET_OPT)

$(IMILIB): header parser $(OBJS)
	@ echo [MKLIB] $@
	@ $(OCAMLC) -a -o $@ $(OBJS)

$(IMILIB_OPT): header parser $(OBJS_OPT)  
	@ echo [MKLIB] $@
	@ $(OCAMLOPT) -a -o $@ $(OBJS_OPT)

$(TARGET): $(IMILIB) $(MAIN)
	@ echo [LINK] $(TARGET)
# 	@ $(OCAMLC) -o $(TARGET) $(INCLUDE) $(LIBS) $(IMILIB) $(MAIN)
	@ $(OCAMLC) -custom -o $(TARGET) $(INCLUDE) $(LIBS) $(IMILIB) $(MAIN)


$(TARGET_OPT): $(IMILIB_OPT) $(MAIN_OPT)
	@ echo [LINK] $(TARGET_OPT)
	@ $(OCAMLOPT) -o $(TARGET_OPT) $(INCLUDE) $(OPTLIBS) $(IMILIB_OPT) $(MAIN_OPT)

header: $(FILESMLI:+=cmi)

parser: $(PARSERS:+=ml) $(LEXERS:+=ml) header $(PARSERS:+=cmi)

$(SRC)/%.cmo: $(SRC)/%.ml $(SRC)/%.mli
	@ echo [OCAMLC] $<
	@ $(OCAMLC) -c $(INCLUDE) $<	

$(SRC)/%.cmo: $(SRC)/%.ml
	@ echo [OCAMLC] $<
	@ $(OCAMLC) -c $(INCLUDE) $<	

$(SRC)/%.cmx: $(SRC)/%.ml $(SRC)/%.mli
	@ echo [OCAMLOPT] $<
	@ $(OCAMLOPT) -c $(INCLUDE) $<	

$(SRC)/%.cmx: $(SRC)/%.ml
	@ echo [OCAMLOPT] $<
	@ $(OCAMLOPT) -c $(INCLUDE) $<	

$(SRC)/%.cmi: $(SRC)/%.mli
	@ echo [OCAMLC] $<
	@ $(OCAMLC) -c $(INCLUDE) $<

$(SRC)/%.cmi: $(SRC)/%.mly
	@ echo [YACC] $<
	@ ocamlyacc $<
	@ echo [OCAMLC] $(SRC)/$*.mli
	@ $(OCAMLC) -c $(INCLUDE) $(SRC)/$*.mli

$(SRC)/%.ml: $(SRC)/%.mly 
	@ echo [YACC] $<
	@ ocamlyacc $<

$(SRC)/%.ml: $(SRC)/%.mll 
	@ echo [LEX] $<
	@ ocamllex $< 

# dependencies
.depend:
	@ echo [OCAMLDEP]
	@ ocamldep -I $(SRC) $(SRC)/*.ml $(SRC)/*.mli > .depend 


test: $(IMILIB) 
	cd test; make unit

.PHONY : doc
doc:
	cd doc; make pdf

exe:

##### GATES #####

# 	$(TARGET) Examples/Gates/NorGate.imi -mode reachability -with-parametric-log

##### TESTS #####

# 	./IMITATOR Examples/exCTL.imi Examples/exCTL.pi0

##### SCHEDULING #####

# 	$(TARGET) $(EXAMPLE_PATH)/CPR08/full_cpr08.im3 $(EXAMPLE_PATH)/CPR08/full_cpr08.pi0 -no-dot -no-log -inclusion
	$(TARGET) $(EXAMPLE_PATH)/LA02/LA02_2.imi $(EXAMPLE_PATH)/LA02/LA02_2.pi0 -no-dot -no-log -inclusion -merge


count: clean
	@ for f in src/*.ml src/*.mli; do wc -l $$f; done | sort -n -r -
#	make clean
#	python lineCounter.py


clean: rmtpf rmuseless
	@rm -rf $(LEXERS:+=ml) $(PARSERS:+=mli) $(PARSERS:+=ml)
	@rm -rf $(TARGET) $(IMILIB)
	@rm -rf .depend
	@cd test; make clean


rmtpf:
	@rm -rf *~


rmuseless:
	@rm -rf $(FILES:+=cmx) $(FILES:+=cmo) $(FILES:+=cmi) $(FILES:+=o) $(MAIN) $(MAIN:.cmo=.cmi) $(MAIN:.cmo=.cmx)
	@rm -rf $(FILESMLI:+=cmi)


include .depend
