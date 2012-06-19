###############################################################
#
#                    HYMITATOR
#
#  Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
#  HYMITATOR is a fork of IMITATOR (split occured in 2010-2011)
#
#  Author:        Etienne Andre, Ulrich KUEHNE, Romain SOULAT
#  Created:       2009/09/07
#  Last modified: 2012/06/18
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

# native c libraries (updated 2012/06/18)
CLIBS = -cclib -lppl

# For 32 bits compiling
STATIC32CLIBS = -cclib '-static -lppl -lcamlrun -ltinfo -lppl_ocaml -lstdc++ -lgmp -lgmpxx'
# CLIBS = -cclib '-static -lppl -lpwl -lcamlrun -ltinfo -lppl_ocaml -lstdc++ -lmlgmp -lmpfr -lgmp -lgmpxx'

# For 64 bits compiling
STATIC64CLIBS = -cclib '-static -ldl -lppl -lcamlrun -ltinfo -lppl_ocaml -lstdc++ -lgmpxx'
# -ldl : inutile
# CLIBS = -cclib '-static -lppl -lpwl -lppl_ocaml -lstdc++ -lmlgmp -lmpfr -lgmp -lgmpxx'


# ocaml lib files
OLIBS = str.cma unix.cma extLib.cma bigarray.cma gmp.cma ppl_ocaml.cma 

# native ocaml lib files
OOLIBS = str.cmxa unix.cmxa extLib.cmxa bigarray.cmxa gmp.cmxa ppl_ocaml.cmxa

# external libs for compiling with PPL support
export LIBS = $(CLIBS) $(OLIBS)
# export OPTLIBS = $(CLIBS) $(OOLIBS) 
export STATIC32LIBS = $(STATIC32CLIBS) $(OLIBS)
export STATIC64LIBS = $(STATIC64CLIBS) $(OLIBS)


SRC = src

# FILES
.PREFIXES : +.
.SUFFIXES : .cmo .cmi .ml .mli .cmxo

# main object
MAIN = $(SRC)/HYMITATOR.cmo
MAIN_OPT = $(MAIN:.cmo=.cmx)

# sources to compile
FILES =  $(SRC)/Global.+ $(SRC)/Cache.+ $(SRC)/Options.+ $(SRC)/NumConst.+  $(SRC)/LinearConstraint.+ $(SRC)/Program.+ $(SRC)/Graphics.+ $(SRC)/Automaton.+ $(SRC)/Pi0Lexer.+ $(SRC)/Pi0Parser.+ $(SRC)/Pi0CubeLexer.+ $(SRC)/Pi0CubeParser.+ $(SRC)/ModelLexer.+ $(SRC)/ModelParser.+ $(SRC)/ModelPrinter.+ $(SRC)/ProgramConverter.+ $(SRC)/ProgramInverter.+ $(SRC)/PredicateAbstraction.+ $(SRC)/Graph.+  $(SRC)/Reachability.+ 
OBJS = $(FILES:+=cmo)
OBJS_OPT = $(OBJS:.cmo=.cmx)

# header files
FILESMLI = $(SRC)/Global.+ $(SRC)/Cache.+ $(SRC)/NumConst.+ $(SRC)/LinearConstraint.+ $(SRC)/Graphics.+ $(SRC)/Automaton.+ $(SRC)/ParsingStructure.+ $(SRC)/AbstractModel.+ $(SRC)/Program.+ $(SRC)/ModelPrinter.+ $(SRC)/ProgramConverter.+ $(SRC)/ProgramInverter.+ $(SRC)/PredicateAbstraction.+ $(SRC)/Graph.+  $(SRC)/Reachability.+  

# parsers and lexers 
LEXERS = $(SRC)/Pi0Lexer.+ $(SRC)/Pi0CubeLexer.+ $(SRC)/ModelLexer.+
PARSERS = $(SRC)/Pi0Parser.+ $(SRC)/Pi0CubeParser.+ $(SRC)/ModelParser.+

# target library
IMILIB = lib/hymitator.cma
IMILIB_OPT = $(IMILIB:.cma=.cmxa)

EXAMPLE_PATH = examples


# target executable
TARGET = bin/HYMITATOR
# TARGET_OPT = bin/HYMITATOR.opt
TARGET_STATIC = bin/HYMITATOR32
TARGET_STATIC64 = bin/HYMITATOR64


default all: $(TARGET)
# opt: $(TARGET_OPT)
static32: $(TARGET_STATIC)
static64: $(TARGET_STATIC64)


$(IMILIB): header parser $(OBJS)
	@ echo [MKLIB] $@
	@ $(OCAMLC) -a -o $@ $(OBJS)

$(IMILIB_OPT): header parser $(OBJS_OPT)  
	@ echo [MKLIB] $@
	@ $(OCAMLOPT) -a -o $@ $(OBJS_OPT)

# $(TARGET): $(IMILIB) $(MAIN)
# 	@ echo [LINK] $(TARGET)
# # 	@ $(OCAMLC) -o $(TARGET) $(INCLUDE) $(LIBS) $(IMILIB) $(MAIN)
# 	@ $(OCAMLC) -custom -o $(TARGET) $(INCLUDE) $(LIBS) $(IMILIB) $(MAIN)
# 
# $(TARGET_OPT): $(IMILIB_OPT) $(MAIN_OPT)
# 	@ echo [LINK] $(TARGET_OPT)
# 	@ $(OCAMLOPT) -o $(TARGET_OPT) $(INCLUDE) $(OPTLIBS) $(IMILIB_OPT) $(MAIN_OPT)

$(TARGET): $(IMILIB) $(MAIN)
	@ echo [LINK] $(TARGET)
	@ $(OCAMLC) $(INCLUDE) $(LIBS) $(IMILIB) $(MAIN) -o $(TARGET)
	
$(TARGET_STATIC): $(IMILIB) $(MAIN)
	@ echo [LINK] $(TARGET_STATIC)
	@ $(OCAMLC) -custom $(INCLUDE) -I $(CLIB_PATH) $(STATIC32LIBS) $(IMILIB) $(MAIN) -o $(TARGET_STATIC) 
	
$(TARGET_STATIC64): $(IMILIB) $(MAIN)
	@ echo [LINK] $(TARGET_STATIC)
	@ $(OCAMLC) -custom $(INCLUDE) -I $(CLIB_PATH) $(STATIC64LIBS) $(IMILIB) $(MAIN) -o $(TARGET_STATIC) 
	


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

##### FISCHER #####

	$(TARGET) $(EXAMPLE_PATH)/Fischer/fischer.hym $(EXAMPLE_PATH)/Fischer/fischer.pi0 -inclusion -fancy

##### JUMPING FROG #####

# 	$(TARGET) $(EXAMPLE_PATH)/Frog/jump.hym $(EXAMPLE_PATH)/Frog/jump.pi0 -inclusion -plot x y -limits x 0 4 -limits y 0 2.5 -fancy

##### NAVIGATION BENCHMARK #####

# 	$(TARGET) $(EXAMPLE_PATH)/Navigation/NAV01.hym -mode reachability -plot x y -limits x 0 3 -limits y 0 3 -fancy
# 	$(TARGET) $(EXAMPLE_PATH)/Navigation/NAV04.hym -mode reachability -plot x y -limits x 0 3 -limits y 0 3 -fancy


##### SCHEDULING #####

# 	$(TARGET) $(EXAMPLE_PATH)/CPR08/full_cpr08.im3 $(EXAMPLE_PATH)/CPR08/full_cpr08.pi0 -no-dot -no-log -inclusion
# 	$(TARGET) $(EXAMPLE_PATH)/LA02/LA02_2.imi $(EXAMPLE_PATH)/LA02/LA02_2.pi0 -no-dot -no-log -inclusion -merge

##### WATER TANK #####

# 	$(TARGET) $(EXAMPLE_PATH)/WaterTank/tank.hym -mode cegar -fancy



count: clean
	@ for f in src/*.ml src/*.mli; do wc -l $$f; done | sort -n -r -


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
