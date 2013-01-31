###############################################################
#
#                    IMITATOR II
#
#  Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
#
#  Author:        Etienne Andre
#  Created:       2009/09/07
#  Last modified: 2013/01/24
#  Ocaml version: 3.12.1
###############################################################

# Flags for ocaml compiler (-g for exception stack trace)
OCAMLC_FLAGS = -g
# OCAMLC_FLAGS = 

# ocaml compiler
OCAMLC = ocamlc $(OCAMLC_FLAGS)
OCAMLOPT = ocamlopt.opt $(OCAMLC_FLAGS)

# path variables
ifndef EXTLIB_PATH
  EXTLIB_PATH = /usr/lib/ocaml/extlib
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

INCLUDE = -I $(SRC) -I $(EXTLIB_PATH) -I $(OCAML_PPL_PATH) -I $(OCAML_GMP_PATH)

# native c libraries (old version)
# CLIBS = -cclib -lpwl -cclib -lm -cclib -lgmpxx -cclib -lgmp -cclib -lppl

# native c libraries (updated 2012/06/07)
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

# Example files for execution directly from the makefile
# EXAMPLE_PATH = /home/etienne/Developpement/IMITATOR2_Examples
EXAMPLE_PATH = examples

# main object
MAIN = $(SRC)/IMITATOR.cmo
# MAIN_OPT = $(MAIN:.cmo=.cmx)

# modules to compile
MODULES = Global NumConst ReachabilityTree Options LinearConstraint Automaton Cache Input Pi0Lexer Pi0Parser V0Lexer V0Parser ModelLexer ModelParser GMLLexer GMLParser ModelPrinter Graph PTA2CLP PTA2GML ProgramConverter Graphics PTA2JPG Reachability Cartography

# interfaces
HEADERS = Global NumConst ReachabilityTree Options LinearConstraint Automaton Cache ParsingStructure AbstractModel Input Graph ModelPrinter PTA2CLP PTA2GML  ProgramConverter Graphics PTA2JPG Reachability Cartography

CMIS = $(addprefix $(SRC)/, $(addsuffix .cmi, $(HEADERS)))
OBJS = $(addprefix $(SRC)/, $(addsuffix .cmo, $(MODULES)))
# OBJS_OPT = $(addprefix $(SRC)/, $(addsuffix .cmx, $(MODULES)))

# parsers and lexers 
LEXERS = Pi0Lexer V0Lexer ModelLexer GMLLexer
PARSERS = Pi0Parser V0Parser ModelParser GMLParser

LEX_ML = $(addprefix $(SRC)/, $(addsuffix .ml, $(LEXERS)))
LEX_CMI = $(addprefix $(SRC)/, $(addsuffix .cmi, $(LEXERS)))
PAR_ML = $(addprefix $(SRC)/, $(addsuffix .ml, $(PARSERS)))
PAR_CMI = $(addprefix $(SRC)/, $(addsuffix .cmi, $(PARSERS)))

# target library
IMILIB = lib/imitator.cma
# IMILIB_OPT = $(IMILIB:.cma=.cmxa)

# target executable
TARGET_PATH = bin/
TARGET_NAME = IMITATOR
VERSION = unstable
TARGET = $(TARGET_PATH)$(TARGET_NAME)
# TARGET_OPT = bin/IMITATOR.opt
TARGET_STATIC32 = bin/IMITATOR32
TARGET_STATIC64 = bin/IMITATOR64


default all: $(TARGET)
# opt: $(TARGET_OPT)
static32: $(TARGET_STATIC32)
static64: $(TARGET_STATIC64)


header: $(CMIS)
parser: $(PAR_ML) $(LEX_ML) header $(PAR_CMI)


$(IMILIB): header parser $(OBJS)
	@ echo [MKLIB] $@
	@ $(OCAMLC) -a -o $@ $(OBJS)  

# $(IMILIB_OPT): header parser $(OBJS_OPT)  
# 	@ echo [MKLIB] $@
# 	@ $(OCAMLOPT) -a -o $@ $(OBJS_OPT)


# $(TARGET): $(MAIN)
# 	@ echo [LINK] $(TARGET)
# 	@ $(OCAMLC) -custom -o $(TARGET) $(INCLUDE) $(LIBS) $(MAIN)
	
$(TARGET): $(IMILIB) $(MAIN)
	@ echo [LINK] $(TARGET)
	@ $(OCAMLC) $(INCLUDE) $(LIBS) $(IMILIB) $(MAIN) -o $(TARGET)
	@ echo [LN] $(TARGET_PATH)$(TARGET_NAME)$(VERSION)
	@ cd $(TARGET_PATH) ; rm $(TARGET_NAME)$(VERSION) ; ln $(TARGET_NAME) $(TARGET_NAME)$(VERSION) -s
	
$(TARGET_STATIC32): $(IMILIB) $(MAIN)
	@ echo [LINK] $(TARGET_STATIC32)
	@ $(OCAMLC) -custom $(INCLUDE) -I $(CLIB_PATH) $(STATIC32LIBS) $(IMILIB) $(MAIN) -o $(TARGET_STATIC32) 
	
$(TARGET_STATIC64): $(IMILIB) $(MAIN)
	@ echo [LINK] $(TARGET_STATIC64)
	@ $(OCAMLC) -custom $(INCLUDE) -I $(CLIB_PATH) $(STATIC64LIBS) $(IMILIB) $(MAIN) -o $(TARGET_STATIC64)

	
	# $(TARGET_OPT): $(IMILIB_OPT) $(MAIN_OPT)
# 	@ echo [LINK] $(TARGET_OPT)
# 	$(OCAMLOPT) -o $(TARGET_OPT) $(INCLUDE) $(OPTLIBS) $(IMILIB_OPT) $(MAIN_OPT)

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
# .depend:
# 	@ echo [OCAMLDEP]
# 	@ ocamldep -I $(SRC) $(SRC)/*.ml $(SRC)/*.mli > .depend 


test:	all
	make exe

testUlrich: $(IMILIB) 
	cd test; make unit

tests:
	python testing/test.py

exe:

##### GATES #####

# 	./IMITATOR Examples/Gates/NorGate.imi -mode reachability -with-parametric-log

##### TRAINS #####

# 	$(TARGET) $(EXAMPLE_PATH)/Train/Train1PTA.imi -mode reachability -incl
# 	$(TARGET) $(EXAMPLE_PATH)/Train/Train1PTA.imi $(EXAMPLE_PATH)/Train/Train1PTA.pi0
# 	$(TARGET) $(EXAMPLE_PATH)/Train/Train1PTA.imi -PTA2GML
# 	$(TARGET) $(EXAMPLE_PATH)/Train/Train1PTA.gml -fromGML -mode reachability

##### TESTS FOR SYNTAX #####

# 	$(TARGET) $(EXAMPLE_PATH)/Tests/test.gml -mode reachability -verbose high -fromGML

# 	$(TARGET) $(EXAMPLE_PATH)/Tests/test.imi -PTA2GML

# 	$(TARGET) $(EXAMPLE_PATH)/Tests/testPost.imi -mode reachability
# 	$(TARGET) $(EXAMPLE_PATH)/Tests/testPostSW.imi -mode reachability
# 	bin/IMITATOR2.4 $(EXAMPLE_PATH)/Tests/testPostSW.imi -mode reachability
# 	$(TARGET) $(EXAMPLE_PATH)/Tests/testPostUpdates.imi -mode reachability -no-merging
# 	bin/IMITATOR2.4 $(EXAMPLE_PATH)/Tests/testPostUpdates.imi -mode reachability
# 	$(TARGET) $(EXAMPLE_PATH)/Tests/testPostUpdates1PTA.imi -mode reachability
# 	$(TARGET) $(EXAMPLE_PATH)/Tests/testPostSW.imi -mode reachability -verbose total -depth-limit 2
# 	$(TARGET) $(EXAMPLE_PATH)/Tests/testPostSansDiscrete.imi -mode reachability -statistics -verbose total

# 	$(TARGET) $(EXAMPLE_PATH)/Tests/exPourGML.imi -PTA2GML

# 	$(TARGET) $(EXAMPLE_PATH)/Tests/exPourGML.imi -mode reachability
# 	$(TARGET) $(EXAMPLE_PATH)/Tests/SynthesizedGML.gml -fromGML -mode reachability

# 	$(TARGET) $(EXAMPLE_PATH)/Tests/model2.gml -fromGML -forcePi0
# 	$(TARGET) $(EXAMPLE_PATH)/Tests/model2.imi -forcePi0

# 	$(TARGET) $(EXAMPLE_PATH)/Tests/model2.imi $(EXAMPLE_PATH)/Tests/model2.pi0 -verbose total
# # 	bin/IMITATOR32romain $(EXAMPLE_PATH)/Tests/model2.imi $(EXAMPLE_PATH)/Tests/model2.pi0 -verbose total
	
# 	$(TARGET) $(EXAMPLE_PATH)/Tests/model2.imi $(EXAMPLE_PATH)/Tests/model2.pi0 -verbose total

# 	$(TARGET) $(EXAMPLE_PATH)/Tests/testCosts.imi $(EXAMPLE_PATH)/Tests/testCosts.pi0 -verbose total -bab -time-limit 1
##### TESTS FOR PROPERTIES #####

# 	$(TARGET) $(EXAMPLE_PATH)/Proprietes/exCTL.imi -mode reachability -with-dot -with-log
# 	$(TARGET) $(EXAMPLE_PATH)/Proprietes/exCTL.imi $(EXAMPLE_PATH)/Proprietes/exCTL.pi0
# 	$(TARGET) $(EXAMPLE_PATH)/Proprietes/exCTL.imi $(EXAMPLE_PATH)/Proprietes/exCTL.pi0 -PTA2CLP

# 	$(TARGET) $(EXAMPLE_PATH)/Proprietes/contrexTermination.imi -mode reachability -no-merging -with-dot -with-log -with-parametric-log
# 	$(TARGET) $(EXAMPLE_PATH)/Proprietes/contrexTermination.imi $(EXAMPLE_PATH)/Proprietes/contrexTermination.pi0 -with-parametric-log -with-log -states-limit 8 -with-dot

# 	$(TARGET) $(EXAMPLE_PATH)/Proprietes/contrexTerminationGlobalClockIMincl.imi -mode reachability -no-merging -incl
# 	$(TARGET) $(EXAMPLE_PATH)/Proprietes/contrexTerminationGlobalClockIMincl.imi $(EXAMPLE_PATH)/Proprietes/contrexTerminationGlobalClockIMincl.pi0 -no-merging -incl -with-parametric-log -with-log -states-limit 30 -with-dot

# 	$(TARGET) $(EXAMPLE_PATH)/Proprietes/exNonTerminationDFS.imi $(EXAMPLE_PATH)/Proprietes/exNonTerminationDFS.pi0 -bab -no-merging -incl -with-log -states-limit 30 -with-parametric-log # PROBLEM BAB
# 	$(TARGET) $(EXAMPLE_PATH)/Proprietes/exNonTerminationDFS.imi $(EXAMPLE_PATH)/Proprietes/exNonTerminationDFS.pi0 -no-merging -incl -with-parametric-log -with-log -states-limit 30 -with-dot

# 	$(TARGET) $(EXAMPLE_PATH)/Examples/testBoucleAvecDiscrete.imi -mode reachability -statistics -depth-limit 200 -no-dot -no-log
# 	$(TARGET) $(EXAMPLE_PATH)/Examples/testBoucleAvecDiscrete.imi -mode reachability -statistics -depth-limit 200 -no-dot -no-log -dynamic
# 	$(TARGET) $(EXAMPLE_PATH)/Examples/testBoucleAvecDiscrete.imi -mode reachability -depth-limit 200 -no-dot -no-log
# 	bin/IMITATOR2.41 $(EXAMPLE_PATH)/Examples/testBoucleAvecDiscrete.imi -mode reachability -depth-limit 200 -no-dot -no-log
# 	bin/IMITATOR2.4 $(EXAMPLE_PATH)/Examples/testBoucleAvecDiscrete.imi -mode reachability -depth-limit 200 -no-dot -no-log -jobshop
# 	bin/IMITATOR2.375 $(EXAMPLE_PATH)/Examples/testBoucleAvecDiscrete.imi -mode reachability -depth-limit 200 -no-dot -no-log -jobshop
# 	bin/IMITATOR2.374 $(EXAMPLE_PATH)/Examples/testBoucleAvecDiscrete.imi -mode reachability -depth-limit 200 -no-dot -no-log -jobshop
# 	bin/IMITATOR2.370 $(EXAMPLE_PATH)/Examples/testBoucleAvecDiscrete.imi -mode reachability -statistics -depth-limit 200 -no-dot -no-log
# 	bin/IMITATOR2.36 $(EXAMPLE_PATH)/Examples/testBoucleAvecDiscrete.imi -mode reachability -statistics -depth-limit 200 -no-dot -no-log
# 	bin/IMITATOR2.35 $(EXAMPLE_PATH)/Examples/testBoucleAvecDiscrete.imi -mode reachability -statistics -depth-limit 200 -no-dot -no-log
# 	bin/IMITATOR2.34.111115 $(EXAMPLE_PATH)/Examples/testBoucleAvecDiscrete.imi -mode reachability -depth-limit 200 -no-dot -no-log

##### CASE STUDIES : EXAMPLES #####
# 	$(TARGET) $(EXAMPLE_PATH)/Examples/exSITH.imi -mode reachability -PTA2GrML
# 	$(TARGET) $(EXAMPLE_PATH)/Examples/exSITH.imi $(EXAMPLE_PATH)/Examples/exSITH.pi0 -bab -with-merging
# 	$(TARGET) $(EXAMPLE_PATH)/Examples/exSITH.imi $(EXAMPLE_PATH)/Examples/exSITH.pi0 -bab
# 	$(TARGET) $(EXAMPLE_PATH)/Examples/exSITH.imi $(EXAMPLE_PATH)/Examples/exSITH.pi0

# 	$(TARGET) $(EXAMPLE_PATH)/Examples/contrexPPTA.imi $(EXAMPLE_PATH)/Examples/contrexPPTA.pi0
# 	$(TARGET) $(EXAMPLE_PATH)/Examples/contrexPPTA.imi $(EXAMPLE_PATH)/Examples/contrexPPTA.pi0 -bab

# 	$(TARGET) $(EXAMPLE_PATH)/Examples/contrexPPTA2.imi -mode reachability
# 	$(TARGET) $(EXAMPLE_PATH)/Examples/contrexPPTA2.imi $(EXAMPLE_PATH)/Examples/contrexPPTA2.pi0
# 	$(TARGET) $(EXAMPLE_PATH)/Examples/contrexPPTA2.imi $(EXAMPLE_PATH)/Examples/contrexPPTA2.pi0 -bab
 	

# 	$(TARGET) $(EXAMPLE_PATH)/Examples/contrexPPTA3.imi $(EXAMPLE_PATH)/Examples/contrexPPTA.pi0
# 	$(TARGET) $(EXAMPLE_PATH)/Examples/contrexPPTA3.imi $(EXAMPLE_PATH)/Examples/contrexPPTA.pi0 -bab

##### CASE STUDIES : HARDWARE #####

# 	$(TARGET) $(EXAMPLE_PATH)/GML/testtemp.grml -fromGrML -mode reachability
# 	$(TARGET) $(EXAMPLE_PATH)/GML/pta1.grml -fromGrML -mode reachability
# 	$(TARGET) $(EXAMPLE_PATH)/AndOr/AndOr.imi -PTA2GrML
# 	$(TARGET) $(EXAMPLE_PATH)/AndOr/AndOr.imi.grml -fromGrML -mode reachability

# 	$(TARGET) $(EXAMPLE_PATH)/AndOr/AndOr.imi -mode reachability -dynamic-elimination -verbose total
# 	$(TARGET) $(EXAMPLE_PATH)/AndOr/AndOr2.imi -PTA2JPG
# 	$(TARGET) $(EXAMPLE_PATH)/AndOr/AndOr.imi -mode reachability -states-limit 10
# 	$(TARGET) $(EXAMPLE_PATH)/AndOr/AndOr.imi -mode reachability -time-limit 10

# 	$(TARGET) $(EXAMPLE_PATH)/AndOr/AndOr2.imi $(EXAMPLE_PATH)/AndOr/AndOr.pi0
# 	$(TARGET) $(EXAMPLE_PATH)/AndOr/AndOr2.imi $(EXAMPLE_PATH)/AndOr/AndOr.pi0 -dynamic-elimination -verbose low

# 	$(TARGET) $(EXAMPLE_PATH)/AndOr/AndOr.imi $(EXAMPLE_PATH)/AndOr/AndOr.pi0
# 	$(TARGET) $(EXAMPLE_PATH)/AndOr/AndOr.imi $(EXAMPLE_PATH)/AndOr/AndOr.pi0 -bab
# 	$(TARGET) $(EXAMPLE_PATH)/AndOr/AndOr.imi $(EXAMPLE_PATH)/AndOr/AndOr.pi0 -with-dot -with-dot-source -fancy
# 	$(TARGET) $(EXAMPLE_PATH)/AndOr/AndOr.imi $(EXAMPLE_PATH)/AndOr/AndOr.pi0 -no-dot -no-log -statistics 
# 	$(TARGET) $(EXAMPLE_PATH)/AndOr/AndOr.imi $(EXAMPLE_PATH)/AndOr/AndOr.pi0 -no-dot -no-log -statistics -dynamic
# 	$(TARGET) $(EXAMPLE_PATH)/AndOr/AndOr.imi $(EXAMPLE_PATH)/AndOr/AndOr.pi0 -no-dot -no-log -jobshop
# 	bin/IMITATOR2.4 $(EXAMPLE_PATH)/AndOr/AndOr.imi $(EXAMPLE_PATH)/AndOr/AndOr.pi0 -no-dot -no-log -jobshop
# 	bin/IMITATOR2.375 $(EXAMPLE_PATH)/AndOr/AndOr.imi $(EXAMPLE_PATH)/AndOr/AndOr.pi0 -no-dot -no-log -jobshop
# 	bin/IMITATOR2.370 $(EXAMPLE_PATH)/AndOr/AndOr.imi $(EXAMPLE_PATH)/AndOr/AndOr.pi0 -no-dot -no-log -statistics

	# 	$(TARGET) $(EXAMPLE_PATH)/AndOr/AndOr.imi $(EXAMPLE_PATH)/AndOr/AndOr.v0 -mode cover

# 	$(TARGET) $(EXAMPLE_PATH)/Flipflop/flipflop.imi -mode reachability 
# 	bin/IMITATOR2.4 $(EXAMPLE_PATH)/Flipflop/flipflop.imi -mode reachability -no-dot -no-log -jobshop
# 	bin/IMITATOR2.375 $(EXAMPLE_PATH)/Flipflop/flipflop.imi -mode reachability -no-dot -no-log -jobshop
# 	bin/IMITATOR2.374 $(EXAMPLE_PATH)/Flipflop/flipflop.imi -mode reachability -no-dot -no-log -jobshop
# 	bin/IMITATOR2.371 $(EXAMPLE_PATH)/Flipflop/flipflop.imi -mode reachability -no-dot -no-log -statistics
# 	bin/IMITATOR2.370 $(EXAMPLE_PATH)/Flipflop/flipflop.imi -mode reachability
# 	bin/IMITATOR2.35.111115 $(EXAMPLE_PATH)/Flipflop/flipflop.imi -mode reachability
# 	bin/IMITATOR2.35 $(EXAMPLE_PATH)/Flipflop/flipflop.imi -mode reachability -no-dot -no-log -statistics
# 	bin/IMITATOR2.34.111115 $(EXAMPLE_PATH)/Flipflop/flipflop.imi -mode reachability -no-dot -no-log 

# 	$(TARGET) $(EXAMPLE_PATH)/Flipflop/flipflop.imi -mode reachability -tree -verbose low
# 	$(TARGET) $(EXAMPLE_PATH)/Flipflop/flipflop.imi -mode reachability -acyclic -verbose low
# 		$(TARGET) $(EXAMPLE_PATH)/Flipflop/flipflop.imi $(EXAMPLE_PATH)/Flipflop/flipflop.pi0 -no-dot -no-log -jobshop -no-random
# 		bin/IMITATOR2.4 $(EXAMPLE_PATH)/Flipflop/flipflop.imi $(EXAMPLE_PATH)/Flipflop/flipflop.pi0 -no-dot -no-log -jobshop -no-random
# 	$(TARGET) $(EXAMPLE_PATH)/Flipflop/flipflop.imi $(EXAMPLE_PATH)/Flipflop/flipflop.pi0
# 	$(TARGET) $(EXAMPLE_PATH)/Flipflop/flipflop.imi $(EXAMPLE_PATH)/Flipflop/flipflop.pi0 -bab
# 	$(TARGET) $(EXAMPLE_PATH)/Flipflop/flipflop.imi $(EXAMPLE_PATH)/Flipflop/flipflop.pi0 -no-dot -no-log -statistics -acyclic
# 	$(TARGET) $(EXAMPLE_PATH)/Flipflop/flipflop.imi $(EXAMPLE_PATH)/Flipflop/flipflop.pi0 -no-dot -no-log -statistics -tree
# 	$(TARGET) $(EXAMPLE_PATH)/Flipflop/flipflop.imi $(EXAMPLE_PATH)/Flipflop/flipflop.pi0 -no-dot -no-log -statistics -dynamic
# 	$(TARGET) $(EXAMPLE_PATH)/Flipflop/flipflop.imi $(EXAMPLE_PATH)/Flipflop/flipflop.pi0 -no-dot -no-log -statistics -jobshop

# 	$(TARGET) $(EXAMPLE_PATH)/Flipflop/flipflop.imi $(EXAMPLE_PATH)/Flipflop/flipflop.v0 -mode cover -cart
# 	$(TARGET) $(EXAMPLE_PATH)/Flipflop/flipflop.imi $(EXAMPLE_PATH)/Flipflop/flipflop.v0 -mode border -cart
# 	./IMITATOR Examples/Flipflop/flipflop.imi Examples/Flipflop/flipflop.v0 -mode random1000  -log-prefix Examples/Flipflop/test2/test
# 	./IMITATOR Examples/Flipflop/flipflop.imi Examples/Flipflop/flipflop.v0 -mode cover
# 	# 	./IMITATOR Examples/Flipflop/flipflop.imi Examples/Flipflop/flipflop.v0 -mode cover -no-log -time-limit 1 -depth-limit 25
# 	./IMITATOR Examples/Flipflop/flipflop.imi Examples/Flipflop/flipflop.v0 -mode cover -no-log -no-dot

# 	$(TARGET) $(EXAMPLE_PATH)/Flipflop/flipflop-observer.imi $(EXAMPLE_PATH)/Flipflop/flipflop.v0 -mode cover -cart
	OCAMLRUNPARAM=b $(TARGET) $(EXAMPLE_PATH)/Flipflop/flipflop-observer.imi $(EXAMPLE_PATH)/Flipflop/flipflop.v0 -mode border -cart

# 	./IMITATOR Examples/Flipflop/flipflop_CC.imi -mode reachability -with-parametric-log
# 	./IMITATOR Examples/Flipflop/flipflop_CC.imi Examples/Flipflop/flipflop_CC.pi0
# 	./IMITATOR Examples/Flipflop/flipflop_CC.imi Examples/Flipflop/flipflop_CC.v0 -mode cover -log-prefix Examples/Flipflop/carto1/carto

# 	$(TARGET) $(EXAMPLE_PATH)/Flipflop-inverse/flipflop-inverse.imi $(EXAMPLE_PATH)/Flipflop-inverse/flipflop-inverse.imi -no-dot -no-log
	
# 	$(TARGET) $(EXAMPLE_PATH)/Latch/latchValmem.imi -mode reachability -no-dot -no-log 
# 	bin/IMITATOR2.375 $(EXAMPLE_PATH)/Latch/latchValmem.imi -mode reachability -no-dot -no-log -jobshop
# 	bin/IMITATOR2.374 $(EXAMPLE_PATH)/Latch/latchValmem.imi -mode reachability -no-dot -no-log -jobshop
# 	bin/IMITATOR2.371 $(EXAMPLE_PATH)/Latch/latchValmem.imi -mode reachability -no-dot -no-log -statistics
# 	bin/IMITATOR2.370 $(EXAMPLE_PATH)/Latch/latchValmem.imi -mode reachability -no-dot -no-log -statistics
# 	bin/IMITATOR2.35 $(EXAMPLE_PATH)/Latch/latchValmem.imi -mode reachability -no-dot -no-log -statistics
# 	bin/IMITATOR2.34.111115 $(EXAMPLE_PATH)/Latch/latchValmem.imi -mode reachability -no-dot -no-log

# 	$(TARGET) $(EXAMPLE_PATH)/Latch/latchValmem.imi $(EXAMPLE_PATH)/Latch/latchValmem.pi0
# 	$(TARGET) $(EXAMPLE_PATH)/Latch/latchValmem.imi $(EXAMPLE_PATH)/Latch/latchValmem.pi0 -bab

# 	$(TARGET) $(EXAMPLE_PATH)/SRlatch/SRlatch.imi -mode reachability
# 	$(TARGET) $(EXAMPLE_PATH)/SRlatch/SRlatch.imi $(EXAMPLE_PATH)/SRlatch/SRlatch.pi0 -with-dot-source -with-dot
# 	$(TARGET) $(EXAMPLE_PATH)/SRlatch/SRlatch.imi $(EXAMPLE_PATH)/SRlatch/SRlatch.pi0 -bab
# 	./IMITATOR $(EXAMPLE_PATH)/SRlatch/SRlatch.imi $(EXAMPLE_PATH)/SRlatch/SRlatch.v0 -mode cover

# 	./IMITATOR Examples/SRlatch/SRlatch_delais_fixes.imi -mode reachability
# 	./IMITATOR Examples/SRlatch/SRlatch_delais_fixes.imi Examples/SRlatch/SRlatch_delais_fixes.pi0
# 	./IMITATOR Examples/SRlatch/SRlatch_delais_fixes.imi Examples/SRlatch/SRlatch_delais_fixes.v0 -mode cover -no-dot

# 	./IMITATOR Examples/SRlatch/sr_latch.hy -mode reachability
# 	./IMITATOR Examples/SRlatch/sr_latch_nand.hy -mode reachability


##### CASE STUDIES : TRAINS #####

# 	$(TARGET) $(EXAMPLE_PATH)/Train/TrainAHV93.imi -mode reachability
# 	bin/IMITATOR2.34.111115 $(EXAMPLE_PATH)/Train/TrainAHV93.imi -mode reachability
# 	
# 	$(TARGET) $(EXAMPLE_PATH)/Train/TrainAHV93.imi $(EXAMPLE_PATH)/Train/TrainAHV93.pi0
# 	$(TARGET) $(EXAMPLE_PATH)/Train/TrainAHV93.imi $(EXAMPLE_PATH)/Train/TrainAHV93.pi0 -incl
# 	$(TARGET) $(EXAMPLE_PATH)/Train/TrainAHV93.imi $(EXAMPLE_PATH)/Train/TrainAHV93.pi0 -bab
# 	bin/IMITATOR2.41 $(EXAMPLE_PATH)/Train/TrainAHV93.imi $(EXAMPLE_PATH)/Train/TrainAHV93.pi0 -incl -no-dot -no-log


##### CASE STUDIES : PROTOCOLS #####

# 	$(TARGET) $(EXAMPLE_PATH)/Fischer/fischerPAT.imi -mode reachability -verbose total
# 	$(TARGET) $(EXAMPLE_PATH)/Fischer/fischerPAT.imi -mode reachability -with-log -with-dot -incl -fancy
# 	$(TARGET) $(EXAMPLE_PATH)/Fischer/fischerPAT.imi -mode reachability -depth-limit 80 -with-log
# 	$(TARGET) $(EXAMPLE_PATH)/Fischer/fischerPAT.imi $(EXAMPLE_PATH)/Fischer/fischerPAT.pi0 -depth-limit 12 -with-log -with-dot
# 	$(TARGET) $(EXAMPLE_PATH)/Fischer/fischerPAT.imi -PTA2JPG
# 	$(TARGET) $(EXAMPLE_PATH)/Fischer/fischerPAT.imi $(EXAMPLE_PATH)/Fischer/fischerPAT.pi0 -depth-limit 20
# 	$(TARGET) $(EXAMPLE_PATH)/Fischer/fischerPAT.imi $(EXAMPLE_PATH)/Fischer/fischerPAT.pi0 -dynamic-elimination -verbose low -depth-limit 20 -with-log 

# 	$(TARGET) $(EXAMPLE_PATH)/BangOlufsen/BangOlufsen.imi -mode reachability 
# 	$(TARGET) $(EXAMPLE_PATH)/BangOlufsen/BangOlufsen.imi $(EXAMPLE_PATH)/BangOlufsen/BangOlufsen.pi0 -depth-limit 12 -incl -statistics
# 	$(TARGET) $(EXAMPLE_PATH)/BangOlufsen/BangOlufsen.imi $(EXAMPLE_PATH)/BangOlufsen/BangOlufsen.pi0 -depth-limit 12 -incl -no-dot -no-log -statistics -dynamic
# 	$(TARGET) $(EXAMPLE_PATH)/BangOlufsen/BangOlufsen.imi $(EXAMPLE_PATH)/BangOlufsen/BangOlufsen.pi0 -depth-limit 12 -incl -no-dot -no-log -statistics -jobshop

# 	./IMITATOR Examples/BangOlufsen/BangOlufsen2.imi -mode reachability -no-dot
# 	./IMITATOR Examples/BangOlufsen/BangOlufsen2.imi -mode reachability -no-dot -depth-limit 30 -verbose low

	
# 	$(TARGET) $(EXAMPLE_PATH)/BRP/brp.imi -PTA2JPG
# 	$(TARGET) $(EXAMPLE_PATH)/BRP/brp.imi -mode reachability 
# 	$(TARGET) $(EXAMPLE_PATH)/BRP/brp.imi $(EXAMPLE_PATH)/BRP/brp.pi0 -statistics
# 	$(TARGET) $(EXAMPLE_PATH)/BRP/brp.imi $(EXAMPLE_PATH)/BRP/brp.pi0
# 	$(TARGET) $(EXAMPLE_PATH)/BRP/brp.imi $(EXAMPLE_PATH)/BRP/brp.pi0 -dynamic-elimination
# 	$(TARGET) $(EXAMPLE_PATH)/BRP/brp.imi $(EXAMPLE_PATH)/BRP/brp.pi0 -bab
# 	$(TARGET) $(EXAMPLE_PATH)/BRP/brp.imi $(EXAMPLE_PATH)/BRP/brp.pi0 -no-dot -no-log -statistics -dynamic

# 	$(TARGET) $(EXAMPLE_PATH)/RCP/RCP.imi -mode reachability -PTA2GML
# 	$(TARGET) $(EXAMPLE_PATH)/RCP/RCP.imi $(EXAMPLE_PATH)/RCP/RCP.pi0
# 	$(TARGET) $(EXAMPLE_PATH)/RCP/RCP.imi $(EXAMPLE_PATH)/RCP/RCP.pi0 -dynamic-elimination
# 	$(TARGET) $(EXAMPLE_PATH)/RCP/RCP.imi $(EXAMPLE_PATH)/RCP/RCP.pi0 -bab # PROBLEM BAB
# 	$(TARGET) $(EXAMPLE_PATH)/RCP/RCP.imi $(EXAMPLE_PATH)/RCP/RCP.pi0 -bab -no-merging
# 	$(TARGET) $(EXAMPLE_PATH)/RCP/RCP.imi $(EXAMPLE_PATH)/RCP/RCP.pi0 -no-dot -no-log -statistics -dynamic

# 	./IMITATOR Examples/RCP/RCP_bounded.imi Examples/RCP/RCP_bounded.pi0 -no-dot -no-log


# 	### WARNING: the prism model seems odd!!
	
# 	$(TARGET) $(EXAMPLE_PATH)/CSMACD/csmacdPrism.imi -PTA2JPG
# 	$(TARGET) $(EXAMPLE_PATH)/CSMACD/csmacdPrism.imi -mode reachability 
# 	$(TARGET) $(EXAMPLE_PATH)/CSMACD/csmacdPrism.imi $(EXAMPLE_PATH)/CSMACD/csmacdPrism.pi0
# 	$(TARGET) $(EXAMPLE_PATH)/CSMACD/csmacdPrism.imi $(EXAMPLE_PATH)/CSMACD/csmacdPrism.pi0 -dynamic-elimination
# 	$(TARGET) $(EXAMPLE_PATH)/CSMACD/csmacdPrism.imi $(EXAMPLE_PATH)/CSMACD/csmacdPrism.pi0 -bab

#	### WARNING: the prism model seems odd!!
# 	$(TARGET) $(EXAMPLE_PATH)/CSMACD/csmacdPrism_with_renamed_actions.imi -mode reachability -no-merging
# 	$(TARGET) $(EXAMPLE_PATH)/CSMACD/csmacdPrism_with_renamed_actions.imi $(EXAMPLE_PATH)/CSMACD/csmacdPrism.pi0 -no-merging

# 	$(TARGET) $(EXAMPLE_PATH)/CSMACD/csmacdPrism_with_renamed_actionsBC5.imi $(EXAMPLE_PATH)/CSMACD/csmacdPrism.pi0 -no-merging
	
# 	$(TARGET) $(EXAMPLE_PATH)/CSMACD/csmacdPrism_with_renamed_actionsBC6.imi $(EXAMPLE_PATH)/CSMACD/csmacdPrism.pi0 -no-merging
# 	$(TARGET) $(EXAMPLE_PATH)/CSMACD/csmacdPrism_with_renamed_actionsBC6.imi $(EXAMPLE_PATH)/CSMACD/csmacdPrism.pi0

# 	$(TARGET) $(EXAMPLE_PATH)/CSMACD/csmacdPrism_with_renamed_actionsBC9.imi $(EXAMPLE_PATH)/CSMACD/csmacdPrism.pi0 -no-merging
# 	$(TARGET) $(EXAMPLE_PATH)/CSMACD/csmacdPrism_with_renamed_actionsBC10.imi $(EXAMPLE_PATH)/CSMACD/csmacdPrism.pi0 -no-merging

	# 	./IMITATOR Examples/CSMACD/csmacdPrism_2p.imi -mode reachability
# 	./IMITATOR Examples/CSMACD/csmacdPrism_2p.imi Examples/CSMACD/csmacdPrism_2p.pi0 
# 	./IMITATOR Examples/CSMACD/csmacdPrism_2p.imi Examples/CSMACD/csmacdPrism_2p.v0 -mode cover -log-prefix Examples/CSMACD/carto_2p/csmacdPrism_2p

# 	$(TARGET) $(EXAMPLE_PATH)/Wlan/wlan.imi -PTA2JPG
# 	$(TARGET) $(EXAMPLE_PATH)/Wlan/wlan.imi -mode reachability
# 	$(TARGET) $(EXAMPLE_PATH)/Wlan/wlan.imi $(EXAMPLE_PATH)/Wlan/wlan.pi0
# 	$(TARGET) $(EXAMPLE_PATH)/Wlan/wlan.imi $(EXAMPLE_PATH)/Wlan/wlan.pi0 -dynamic-elimination
# 	$(TARGET) $(EXAMPLE_PATH)/Wlan/wlan.imi $(EXAMPLE_PATH)/Wlan/wlan.pi0 -no-merging
# 	$(TARGET) $(EXAMPLE_PATH)/Wlan/wlan.imi $(EXAMPLE_PATH)/Wlan/wlan.pi0 -bab
# 	$(TARGET) $(EXAMPLE_PATH)/Wlan/wlan.imi $(EXAMPLE_PATH)/Wlan/wlan.pi0 -depth-limit 10 -no-dot -no-log -statistics 
# 	$(TARGET) $(EXAMPLE_PATH)/Wlan/wlan.imi $(EXAMPLE_PATH)/Wlan/wlan.pi0 -depth-limit 10 -no-dot -no-log -statistics -dynamic
# 	$(TARGET) $(EXAMPLE_PATH)/Wlan/wlan.imi $(EXAMPLE_PATH)/Wlan/wlan.pi0 -depth-limit 10 -no-dot -no-log

# 	$(TARGET) $(EXAMPLE_PATH)/Wlan/wlan2_without_asap.imi $(EXAMPLE_PATH)/Wlan/wlan2_without_asap.pi0
# 	$(TARGET) $(EXAMPLE_PATH)/Wlan/wlan2_without_asap_without_minmax.imi $(EXAMPLE_PATH)/Wlan/wlan2_without_asap_without_minmax.pi0 -no-dot -no-log

# 	./IMITATOR Examples/Wlan/wlan_boff2.imi Examples/Wlan/wlan_boff2.pi0 -timed

# 	./IMITATOR Examples/Wlan/wlan2_for_im2.imi -mode reachability -verbose low
# 	./IMITATOR Examples/Wlan/wlan2_for_im2.imi Examples/Wlan/wlan2_for_im2.pi0 -timed

##### CASE STUDIES : VALMEM #####

# 	$(TARGET) $(EXAMPLE_PATH)/Valmem/spsmall.imi -PTA2JPG 
# 	$(TARGET) $(EXAMPLE_PATH)/Valmem/spsmall.imi -mode reachability -verbose total
# 	$(TARGET) $(EXAMPLE_PATH)/Valmem/spsmall.imi $(EXAMPLE_PATH)/Valmem/spsmall.pi0
# 	$(TARGET) $(EXAMPLE_PATH)/Valmem/spsmall.imi $(EXAMPLE_PATH)/Valmem/spsmall.pi0 -no-random -bab
# 	$(TARGET) $(EXAMPLE_PATH)/Valmem/spsmall.imi $(EXAMPLE_PATH)/Valmem/spsmall.pi0 -with-dot -with-dot-source -fancy
# 	$(TARGET) $(EXAMPLE_PATH)/Valmem/spsmall.imi $(EXAMPLE_PATH)/Valmem/spsmall.pi0 -no-dot -no-log -PTA2GML
# 	$(TARGET) $(EXAMPLE_PATH)/Valmem/spsmall.imi $(EXAMPLE_PATH)/Valmem/spsmall.pi0 -no-dot -no-log -statistics
# 	$(TARGET) $(EXAMPLE_PATH)/Valmem/spsmall.imi $(EXAMPLE_PATH)/Valmem/spsmall.pi0 -no-dot -no-log -statistics -acyclic
# 	$(TARGET) $(EXAMPLE_PATH)/Valmem/spsmall.imi $(EXAMPLE_PATH)/Valmem/spsmall.pi0 -no-dot -no-log -statistics -tree -acyclic
# 	$(TARGET) $(EXAMPLE_PATH)/Valmem/spsmall.imi $(EXAMPLE_PATH)/Valmem/spsmall.pi0 -no-dot -no-log -statistics -dynamic

# 	./IMITATOR Examples/Valmem/spsmall_obs.imi -mode reachability -with-parametric-log
# 	./IMITATOR Examples/Valmem/spsmall_obs.imi Examples/Valmem/spsmall.pi0
# 	./IMITATOR Examples/Valmem/spsmall_obs.imi Examples/Valmem/spsmall.v0 -mode cover -log-prefix Examples/Valmem/carto/spsmall

# 	$(TARGET) $(EXAMPLE_PATH)/Valmem/LSV.imi $(EXAMPLE_PATH)/Valmem/delais1_hy.pi0 -no-dot -no-log -statistics 
# 	$(TARGET) $(EXAMPLE_PATH)/Valmem/LSV.imi $(EXAMPLE_PATH)/Valmem/delais1_hy.pi0 -no-dot -no-log -depth-limit 31 -jobshop -verbose medium
# 	$(TARGET) $(EXAMPLE_PATH)/Valmem/LSV.imi $(EXAMPLE_PATH)/Valmem/delais1_hy.pi0 -no-dot -no-log -statistics -acyclic -dynamic
# 	$(TARGET) $(EXAMPLE_PATH)/Valmem/sp_1x2_md_no.imi Examples/Valmem/sp_1x2_md_no.pi0 

##### CASE STUDIES : SIMOP #####
# 	$(TARGET) $(EXAMPLE_PATH)/SIMOP/simop.imi -PTA2JPG
# 	$(TARGET) $(EXAMPLE_PATH)/SIMOP/simop.imi -mode reachability 

# 	$(TARGET) $(EXAMPLE_PATH)/SIMOP/simop.imi $(EXAMPLE_PATH)/SIMOP/simop.pi0
# 	$(TARGET) $(EXAMPLE_PATH)/SIMOP/simop.imi $(EXAMPLE_PATH)/SIMOP/simop.pi0 -with-merging
# 	$(TARGET) $(EXAMPLE_PATH)/SIMOP/simop.imi $(EXAMPLE_PATH)/SIMOP/simop.pi0 -with-merging -dynamic-elimination
# 	$(TARGET) $(EXAMPLE_PATH)/SIMOP/simop.imi $(EXAMPLE_PATH)/SIMOP/simop.pi0 -no-merging -incl -statistics
# 	$(TARGET) $(EXAMPLE_PATH)/SIMOP/simopTest.imi $(EXAMPLE_PATH)/SIMOP/simopTest.pi0 -bab -no-merging -incl -no-random -with-log -states-limit 30 -with-parametric-log # PROBLEM BAB

# 	$(TARGET) $(EXAMPLE_PATH)/SIMOP/simopTest.imi $(EXAMPLE_PATH)/SIMOP/simopTest.pi0 -no-merging -incl -no-random -with-parametric-log -with-log -states-limit 30 -with-dot

	
# 	$(TARGET) $(EXAMPLE_PATH)/SIMOP/simop.imi $(EXAMPLE_PATH)/SIMOP/simop.pi0 -with-log -with-dot -fancy
# 	$(TARGET) $(EXAMPLE_PATH)/SIMOP/simop.imi $(EXAMPLE_PATH)/SIMOP/simop.pi0 -incl -no-dot -no-log -statistics -dynamic


##### SCHEDULING #####
# 	$(TARGET) $(EXAMPLE_PATH)/Scheduling/am02.imi -PTA2JPG
# 	$(TARGET) $(EXAMPLE_PATH)/Scheduling/am02.imi -mode reachability 
# 	$(TARGET) $(EXAMPLE_PATH)/Scheduling/am02.imi $(EXAMPLE_PATH)/Scheduling/am02.pi0 -with-merging -with-dot -with-log
# 	$(TARGET) $(EXAMPLE_PATH)/Scheduling/am02.imi $(EXAMPLE_PATH)/Scheduling/am02.pi0 -bab # PROBLEM BAB
# 	$(TARGET) $(EXAMPLE_PATH)/Scheduling/am02.imi $(EXAMPLE_PATH)/Scheduling/am02.pi0 -bab -no-merging

# 	$(TARGET) $(EXAMPLE_PATH)/Scheduling/astrium_basic_thermal_fp.imi -PTA2JPG
# 	$(TARGET) $(EXAMPLE_PATH)/Scheduling/astrium_basic_thermal_fp.imi $(EXAMPLE_PATH)/Scheduling/astrium_basic_thermal_fp.pi0 -with-merging -with-dot -with-log

# 	$(TARGET) $(EXAMPLE_PATH)/Scheduling/bb.imi -PTA2JPG
# 	$(TARGET) $(EXAMPLE_PATH)/Scheduling/bb.imi $(EXAMPLE_PATH)/Scheduling/bb.pi0 -with-merging -with-dot -with-log
	
# 	$(TARGET) $(EXAMPLE_PATH)/Scheduling/concurent_tasks_chain.imi -PTA2JPG
# 	$(TARGET) $(EXAMPLE_PATH)/Scheduling/concurent_tasks_chain.imi $(EXAMPLE_PATH)/Scheduling/concurent_tasks_chain.pi0 -with-merging -with-dot -with-log
	
# 	$(TARGET) $(EXAMPLE_PATH)/Scheduling/full_cpr08.imi -PTA2JPG
# 	$(TARGET) $(EXAMPLE_PATH)/Scheduling/full_cpr08.imi $(EXAMPLE_PATH)/Scheduling/full_cpr08.pi0 -with-merging -with-dot -with-log -depth-limit 30

# 	$(TARGET) $(EXAMPLE_PATH)/Scheduling/generic_edf.imi -PTA2JPG
# 	$(TARGET) $(EXAMPLE_PATH)/Scheduling/generic_edf.imi $(EXAMPLE_PATH)/Scheduling/generic_edf.pi0 -with-merging -with-dot -with-log

# 	$(TARGET) $(EXAMPLE_PATH)/Scheduling/generic_fp.imi -PTA2JPG
# 	$(TARGET) $(EXAMPLE_PATH)/Scheduling/generic_fp.imi $(EXAMPLE_PATH)/Scheduling/generic_fp.pi0 -with-merging -with-dot -with-log

# 	$(TARGET) $(EXAMPLE_PATH)/Scheduling/hppr10_audio.imi -PTA2JPG
# 	$(TARGET) $(EXAMPLE_PATH)/Scheduling/hppr10_audio.imi $(EXAMPLE_PATH)/Scheduling/hppr10_audio.pi0 -with-merging -with-dot -with-log
	
# 	$(TARGET) $(EXAMPLE_PATH)/Scheduling/LA02_2.imi -PTA2JPG
# 	$(TARGET) $(EXAMPLE_PATH)/Scheduling/LA02_2.imi $(EXAMPLE_PATH)/Scheduling/LA02_2.pi0 -with-merging -with-dot -with-log
# 	bin/IMITATOR2.41 $(EXAMPLE_PATH)/Scheduling/LA02_2.imi $(EXAMPLE_PATH)/Scheduling/LA02_2.pi0 -incl
# 	bin/IMITATOR2.4 $(EXAMPLE_PATH)/Scheduling/LA02_2.imi $(EXAMPLE_PATH)/Scheduling/LA02_2.pi0 -incl -jobshop

# 	$(TARGET) $(EXAMPLE_PATH)/Scheduling/LA02_2_2.5.imi -PTA2JPG
# 	$(TARGET) $(EXAMPLE_PATH)/Scheduling/LA02_2_2.5.imi $(EXAMPLE_PATH)/Scheduling/LA02_2.pi0 -with-merging -with-dot -with-log

# 	$(TARGET) $(EXAMPLE_PATH)/Scheduling/LA02_3.imi -PTA2JPG
# 	$(TARGET) $(EXAMPLE_PATH)/Scheduling/LA02_3.imi $(EXAMPLE_PATH)/Scheduling/LA02_3.pi0 -with-merging

# 	$(TARGET) $(EXAMPLE_PATH)/Scheduling/preemptive_maler.imi -PTA2JPG
# 	$(TARGET) $(EXAMPLE_PATH)/Scheduling/preemptive_maler.imi $(EXAMPLE_PATH)/Scheduling/preemptive_maler.pi0 -with-merging -with-dot -with-log

# 	$(TARGET) $(EXAMPLE_PATH)/Polyhedra/polyhedron1.imi $(EXAMPLE_PATH)/Polyhedra/polyhedron1.v0 -mode cover -cart

# 	$(TARGET) $(EXAMPLE_PATH)/Others/test.imi $(EXAMPLE_PATH)/Others/test.v0 -mode border -incl -merge -cart -with-dot
# 	$(TARGET) $(EXAMPLE_PATH)/Others/test2.imi $(EXAMPLE_PATH)/Others/test2.v0 -mode cover -incl -merge -cart -with-dot
# 	$(TARGET) $(EXAMPLE_PATH)/Others/giuseppe.imi $(EXAMPLE_PATH)/Others/giuseppe.v0 -mode border -incl -merge -depth-limit 15
# 	$(TARGET) $(EXAMPLE_PATH)/Others/giuseppe3.imi $(EXAMPLE_PATH)/Others/giuseppe3.v0 -mode cover -incl -merge -timed 
# 	$(TARGET) $(EXAMPLE_PATH)/Others/giuseppe_opt.imi $(EXAMPLE_PATH)/Others/giuseppe_opt.v0 -mode cover -incl -merge

##### JOB SHOP #####
# 	$(TARGET) $(EXAMPLE_PATH)/Jobshop/maler_2_4.imi -mode reachability -incl 

# 	$(TARGET) $(EXAMPLE_PATH)/Jobshop/maler_2_4.imi $(EXAMPLE_PATH)/Jobshop/maler_2_4.pi0
# 	$(TARGET) $(EXAMPLE_PATH)/Jobshop/maler_2_4.imi $(EXAMPLE_PATH)/Jobshop/maler_2_4.pi0 -with-merging
# 	$(TARGET) $(EXAMPLE_PATH)/Jobshop/maler_2_4.imi $(EXAMPLE_PATH)/Jobshop/maler_2_4.pi0 -dynamic-elimination
# 	$(TARGET) $(EXAMPLE_PATH)/Jobshop/maler_2_4.imi $(EXAMPLE_PATH)/Jobshop/maler_2_4.pi0 -with-merging -dynamic-elimination
# 	$(TARGET) $(EXAMPLE_PATH)/Jobshop/maler_2_4.imi $(EXAMPLE_PATH)/Jobshop/maler_2_4.pi0 -acyclic -statistics
# 	$(TARGET) $(EXAMPLE_PATH)/Jobshop/maler_2_4.imi $(EXAMPLE_PATH)/Jobshop/maler_2_4.pi0 -incl
# 	$(TARGET) $(EXAMPLE_PATH)/Jobshop/maler_2_4.imi $(EXAMPLE_PATH)/Jobshop/maler_2_4.pi0 -bab # PROBLEM BAB
# 	$(TARGET) $(EXAMPLE_PATH)/Jobshop/maler_2_4.imi $(EXAMPLE_PATH)/Jobshop/maler_2_4.pi0 -no-merging -bab
# 	bin/IMITATOR2.41 $(EXAMPLE_PATH)/Jobshop/maler_2_4.imi $(EXAMPLE_PATH)/Jobshop/maler_2_4.pi0 -no-dot -no-log -incl
# 	bin/IMITATOR2.4 $(EXAMPLE_PATH)/Jobshop/maler_2_4.imi $(EXAMPLE_PATH)/Jobshop/maler_2_4.pi0 -no-dot -no-log -incl -jobshop
# 	bin/IMITATOR2.4 $(EXAMPLE_PATH)/Jobshop/maler_2_4.imi $(EXAMPLE_PATH)/Jobshop/maler_2_4.pi0 -no-dot -no-log -incl -jobshop
# 	bin/IMITATOR2.375 $(EXAMPLE_PATH)/Jobshop/maler_2_4.imi $(EXAMPLE_PATH)/Jobshop/maler_2_4.pi0 -incl -jobshop
# 	bin/IMITATOR2.371 $(EXAMPLE_PATH)/Jobshop/maler_2_4.imi $(EXAMPLE_PATH)/Jobshop/maler_2_4.pi0 -incl -statistics


# 	$(TARGET) $(EXAMPLE_PATH)/Jobshop/maler_3_4.imi -mode reachability -no-dot -no-log -incl -acyclic -statistics 
# 	$(TARGET) $(EXAMPLE_PATH)/Jobshop/maler_3_4.imi -mode reachability -no-dot -no-log -incl -acyclic -statistics -dynamic
# 	$(TARGET) $(EXAMPLE_PATH)/Jobshop/maler_3_4.imi -mode reachability -no-dot -no-log -incl -acyclic -jobshop -statistics
# 	bin/IMITATOR2.4 $(EXAMPLE_PATH)/Jobshop/maler_3_4.imi -mode reachability -no-dot -no-log -incl -acyclic -jobshop -statistics
# 	bin/IMITATOR2.375 $(EXAMPLE_PATH)/Jobshop/maler_3_4.imi -mode reachability -no-dot -no-log -incl -acyclic -jobshop
# 	bin/IMITATOR2.374 $(EXAMPLE_PATH)/Jobshop/maler_3_4.imi -mode reachability -no-dot -no-log -incl -acyclic -jobshop
# 	bin/IMITATOR2.371 $(EXAMPLE_PATH)/Jobshop/maler_3_4.imi -mode reachability -no-dot -no-log -incl -acyclic -statistics -jobshop
# 	bin/IMITATOR2.36 $(EXAMPLE_PATH)/Jobshop/maler_3_4_inst.imi -mode reachability -no-dot -no-log -incl -acyclic -statistics
# 	bin/IMITATOR2.35 $(EXAMPLE_PATH)/Jobshop/maler_3_4.imi -mode reachability -no-dot -no-log -incl -statistics -depth-limit 10
# 	bin/IMITATOR2.34.111115 $(EXAMPLE_PATH)/Jobshop/maler_3_4_inst.imi -mode reachability -no-dot -no-log -IMincl

# 	$(TARGET) $(EXAMPLE_PATH)/Jobshop/maler_3_4.imi $(EXAMPLE_PATH)/Jobshop/maler_3_4.pi0 -incl
# 	bin/IMITATOR2.41 $(EXAMPLE_PATH)/Jobshop/maler_3_4.imi $(EXAMPLE_PATH)/Jobshop/maler_3_4.pi0 -no-dot -no-log -incl
# 	bin/IMITATOR2.4 $(EXAMPLE_PATH)/Jobshop/maler_3_4.imi $(EXAMPLE_PATH)/Jobshop/maler_3_4.pi0 -no-dot -no-log -incl -jobshop
# 	bin/IMITATOR2.375 $(EXAMPLE_PATH)/Jobshop/maler_3_4.imi $(EXAMPLE_PATH)/Jobshop/maler_3_4.pi0 -no-dot -no-log -incl -jobshop
# 	bin/IMITATOR2.371 $(EXAMPLE_PATH)/Jobshop/maler_3_4.imi $(EXAMPLE_PATH)/Jobshop/maler_3_4.pi0 -no-dot -no-log -incl -statistics -depth-limit 9

# 	$(TARGET) $(EXAMPLE_PATH)/Jobshop/maler_3_4_inst.imi -mode reachability -no-dot -no-log -incl -statistics
# 	bin/IMITATOR2.36 $(EXAMPLE_PATH)/Jobshop/maler_3_4_inst.imi -mode reachability -no-dot -no-log -incl -statistics
# 	bin/IMITATOR2.35 $(EXAMPLE_PATH)/Jobshop/maler_3_4.imi_inst -mode reachability -no-dot -no-log -incl -statistics
# 	bin/IMITATOR2.34.111115 $(EXAMPLE_PATH)/Jobshop/maler_3_4_inst.imi -mode reachability -no-dot -no-log -IMincl

# 	$(TARGET) $(EXAMPLE_PATH)/Jobshop/maler_3_4.imi -mode reachability -no-dot -no-log -incl -statistics
# 	bin/IMITATOR2.371 $(EXAMPLE_PATH)/Jobshop/maler_3_4.imi -mode reachability -no-dot -no-log -incl -statistics
# 	bin/IMITATOR2.370 $(EXAMPLE_PATH)/Jobshop/maler_3_4.imi -mode reachability -no-dot -no-log -incl -statistics
# 	bin/IMITATOR2.36 $(EXAMPLE_PATH)/Jobshop/maler_3_4.ancien.imi -mode reachability -no-dot -no-log -incl -statistics
# 	bin/IMITATOR2.35 $(EXAMPLE_PATH)/Jobshop/maler_3_4.imi -mode reachability -no-dot -no-log -incl -statistics
# 	bin/IMITATOR2.34.111115 $(EXAMPLE_PATH)/Jobshop/maler_3_4.imi -mode reachability -no-dot -no-log -IMincl


# 	$(TARGET) $(EXAMPLE_PATH)/Jobshop/maler_4_4.imi -mode reachability -no-dot -no-log -incl -statistics -PTA2GML
# 	$(TARGET) $(EXAMPLE_PATH)/Jobshop/maler_4_4.imi -mode reachability -no-dot -no-log -incl -acyclic -depth-limit 10
# 	bin/IMITATOR2.375 $(EXAMPLE_PATH)/Jobshop/maler_4_4.imi -mode reachability -no-dot -no-log -incl -acyclic -depth-limit 10
# 	bin/IMITATOR2.370 $(EXAMPLE_PATH)/Jobshop/maler_4_4.imi -mode reachability -no-dot -no-log -incl -statistics -depth-limit 8
# 	bin/IMITATOR2.36 $(EXAMPLE_PATH)/Jobshop/maler_4_4.imi -mode reachability -no-dot -no-log -incl -statistics



count: clean
	@ for f in src/*.ml src/*.mli; do wc -l $$f; done | sort -n -r -


clean: rmtpf rmuseless
	@rm -rf $(OBJS) $(OBJS_OPT) $(CMIS)  $(PAR_ML) $(PAR_CMI) $(OBJS:.cmo=.o)
	@rm -rf $(LEX_CMI) $(LEX_ML)
	@rm -rf $(TARGET) $(IMILIB) $(TARGET_OPT) $(IMILIB_OPT)
	@rm -rf .depend
	@cd test; make clean


rmtpf:
	@rm -rf *~


rmuseless:
	@rm -rf $(FILES:+=cmo) $(FILES:+=cmx) $(FILES:+=cmi) $(FILES:+=o) $(MAIN) $(MAIN:.cmo=.cmi) $(MAIN:.cmo=.cmx)
	@rm -rf $(FILESMLI:+=cmi)
 
# include .depend
