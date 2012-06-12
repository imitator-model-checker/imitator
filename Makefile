###############################################################
#
#                    IMITATOR II
#
#  Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
#
#  Author:        Etienne Andre
#  Created:       2009/09/07
#  Last modified: 2012/06/07
#  Ocaml version: 3.12.1
###############################################################

# flags for ocaml compiler
#OCAMLC_FLAGS = -g
OCAMLC_FLAGS = 

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

# FOR STATIC COMPILING IN 32 BITS
STATIC32CLIBS = -cclib '-static -lppl -lcamlrun -ltinfo -lppl_ocaml -lstdc++ -lgmp -lgmpxx'
# (old version)
# CLIBS = -cclib '-static -lppl -lpwl -lcamlrun -ltinfo -lppl_ocaml -lstdc++ -lmlgmp -lmpfr -lgmp -lgmpxx'

# ALLOWS STATIC COMPILING IN 64 BITS :-)
# CLIBS = -cclib '-static -lppl -lpwl -lppl_ocaml -lstdc++ -lmlgmp -lmpfr -lgmp -lgmpxx ' 


# ocaml lib files
OLIBS = str.cma unix.cma extLib.cma bigarray.cma gmp.cma ppl_ocaml.cma 

# native ocaml lib files
OOLIBS = str.cmxa unix.cmxa extLib.cmxa bigarray.cmxa gmp.cmxa ppl_ocaml.cmxa

# external libs for compiling with PPL support
export LIBS = $(CLIBS) $(OLIBS)
export STATIC32LIBS = $(STATIC32CLIBS) $(OLIBS)
# export OPTLIBS = $(CLIBS) $(OOLIBS) 


SRC = src

# Example files for execution directly from the makefile
# EXAMPLE_PATH = /home/etienne/Developpement/IMITATOR2_Examples
EXAMPLE_PATH = examples

# main object
MAIN = $(SRC)/IMITATOR.cmo
# MAIN_OPT = $(MAIN:.cmo=.cmx)

# modules to compile
MODULES = Global NumConst ReachabilityTree Options LinearConstraint Automaton Cache Pi0Lexer Pi0Parser Pi0CubeLexer Pi0CubeParser ImitatorLexer ImitatorParser GMLLexer GMLParser ImitatorPrinter Graph PTA2CLP PTA2GML ProgramConverter Reachability Graphics

# interfaces
HEADERS = Global NumConst ReachabilityTree Options LinearConstraint Automaton Cache ParsingStructure AbstractImitatorFile Graph ImitatorPrinter PTA2CLP PTA2GML ProgramConverter Reachability Graphics

CMIS = $(addprefix $(SRC)/, $(addsuffix .cmi, $(HEADERS)))
OBJS = $(addprefix $(SRC)/, $(addsuffix .cmo, $(MODULES)))
# OBJS_OPT = $(addprefix $(SRC)/, $(addsuffix .cmx, $(MODULES)))

# parsers and lexers 
LEXERS = Pi0Lexer Pi0CubeLexer ImitatorLexer GMLLexer
PARSERS = Pi0Parser Pi0CubeParser ImitatorParser GMLParser

LEX_ML = $(addprefix $(SRC)/, $(addsuffix .ml, $(LEXERS)))
LEX_CMI = $(addprefix $(SRC)/, $(addsuffix .cmi, $(LEXERS)))
PAR_ML = $(addprefix $(SRC)/, $(addsuffix .ml, $(PARSERS)))
PAR_CMI = $(addprefix $(SRC)/, $(addsuffix .cmi, $(PARSERS)))

# target library
IMILIB = lib/imitator.cma
# IMILIB_OPT = $(IMILIB:.cma=.cmxa)

# target executable
TARGET = bin/IMITATOR
# TARGET_OPT = bin/IMITATOR.opt
TARGET_STATIC = bin/IMITATOR32


default all: $(TARGET)
# opt: $(TARGET_OPT)
static32: $(TARGET_STATIC)


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
	
$(TARGET_STATIC): $(IMILIB) $(MAIN)
	@ echo [LINK] $(TARGET_STATIC)
	@ $(OCAMLC) -custom $(INCLUDE) -I $(CLIB_PATH) $(STATIC32LIBS) $(IMILIB) $(MAIN) -o $(TARGET_STATIC) 
	
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

exe:

##### GATES #####

# 	./IMITATOR Examples/Gates/NorGate.imi -mode reachability -with-parametric-log

##### TRAINS #####

# 	$(TARGET) $(EXAMPLE_PATH)/Train/Train1PTA.imi -mode reachability -with-parametric-log -incl
# 	$(TARGET) $(EXAMPLE_PATH)/Train/Train1PTA.imi $(EXAMPLE_PATH)/Train/Train1PTA.pi0
# 	$(TARGET) $(EXAMPLE_PATH)/Train/Train1PTA.imi -PTA2GML
# 	$(TARGET) $(EXAMPLE_PATH)/Train/Train1PTA.gml -fromGML -mode reachability

##### TESTS FOR SYNTAX #####

# 	$(TARGET) $(EXAMPLE_PATH)/Tests/test.gml -mode reachability -debug high -fromGML

# 	$(TARGET) $(EXAMPLE_PATH)/Tests/test.imi -PTA2GML

# 	$(TARGET) $(EXAMPLE_PATH)/Tests/testPost.imi -mode reachability
# 	$(TARGET) $(EXAMPLE_PATH)/Tests/testPostSW.imi -mode reachability
# 	bin/IMITATOR2.4 $(EXAMPLE_PATH)/Tests/testPostSW.imi -mode reachability
# 	$(TARGET) $(EXAMPLE_PATH)/Tests/testPostUpdates.imi -mode reachability -no-merging
# 	bin/IMITATOR2.4 $(EXAMPLE_PATH)/Tests/testPostUpdates.imi -mode reachability
# 	$(TARGET) $(EXAMPLE_PATH)/Tests/testPostUpdates1PTA.imi -mode reachability
# 	$(TARGET) $(EXAMPLE_PATH)/Tests/testPostSW.imi -mode reachability -debug total -post-limit 2
# 	$(TARGET) $(EXAMPLE_PATH)/Tests/testPostSansDiscrete.imi -mode reachability -statistics -debug total

# 	$(TARGET) $(EXAMPLE_PATH)/Tests/exPourGML.imi -PTA2GML

# 	$(TARGET) $(EXAMPLE_PATH)/Tests/exPourGML.imi -mode reachability
# 	$(TARGET) $(EXAMPLE_PATH)/Tests/SynthesizedGML.gml -fromGML -mode reachability

# 	$(TARGET) $(EXAMPLE_PATH)/Tests/testCosts.imi $(EXAMPLE_PATH)/Tests/testCosts.pi0 -debug total -bab
##### TESTS FOR PROPERTIES #####

# 	$(TARGET) $(EXAMPLE_PATH)/Proprietes/exCTL.imi $(EXAMPLE_PATH)/Proprietes/exCTL.pi0
# 	$(TARGET) $(EXAMPLE_PATH)/Proprietes/exCTL.imi $(EXAMPLE_PATH)/Proprietes/exCTL.pi0 -PTA2CLP

# 	$(TARGET) $(EXAMPLE_PATH)/Examples/testBoucleAvecDiscrete.imi -mode reachability -statistics -post-limit 200 -no-dot -no-log
# 	$(TARGET) $(EXAMPLE_PATH)/Examples/testBoucleAvecDiscrete.imi -mode reachability -statistics -post-limit 200 -no-dot -no-log -dynamic
# 	$(TARGET) $(EXAMPLE_PATH)/Examples/testBoucleAvecDiscrete.imi -mode reachability -post-limit 200 -no-dot -no-log
# 	bin/IMITATOR2.41 $(EXAMPLE_PATH)/Examples/testBoucleAvecDiscrete.imi -mode reachability -post-limit 200 -no-dot -no-log
# 	bin/IMITATOR2.4 $(EXAMPLE_PATH)/Examples/testBoucleAvecDiscrete.imi -mode reachability -post-limit 200 -no-dot -no-log -jobshop
# 	bin/IMITATOR2.375 $(EXAMPLE_PATH)/Examples/testBoucleAvecDiscrete.imi -mode reachability -post-limit 200 -no-dot -no-log -jobshop
# 	bin/IMITATOR2.374 $(EXAMPLE_PATH)/Examples/testBoucleAvecDiscrete.imi -mode reachability -post-limit 200 -no-dot -no-log -jobshop
# 	bin/IMITATOR2.370 $(EXAMPLE_PATH)/Examples/testBoucleAvecDiscrete.imi -mode reachability -statistics -post-limit 200 -no-dot -no-log
# 	bin/IMITATOR2.36 $(EXAMPLE_PATH)/Examples/testBoucleAvecDiscrete.imi -mode reachability -statistics -post-limit 200 -no-dot -no-log
# 	bin/IMITATOR2.35 $(EXAMPLE_PATH)/Examples/testBoucleAvecDiscrete.imi -mode reachability -statistics -post-limit 200 -no-dot -no-log
# 	bin/IMITATOR2.34.111115 $(EXAMPLE_PATH)/Examples/testBoucleAvecDiscrete.imi -mode reachability -post-limit 200 -no-dot -no-log

##### CASE STUDIES : EXAMPLES #####
# 	$(TARGET) $(EXAMPLE_PATH)/Examples/exSITH.imi -mode reachability -PTA2GML
# 	$(TARGET) $(EXAMPLE_PATH)/Examples/exSITH.imi $(EXAMPLE_PATH)/Examples/exSITH.pi0

# 	$(TARGET) $(EXAMPLE_PATH)/Examples/contrexPPTA.imi $(EXAMPLE_PATH)/Examples/contrexPPTA.pi0

# 	$(TARGET) $(EXAMPLE_PATH)/Examples/contrexPPTA2.imi -mode reachability
# 	$(TARGET) $(EXAMPLE_PATH)/Examples/contrexPPTA2.imi $(EXAMPLE_PATH)/Examples/contrexPPTA2.pi0

# 	$(TARGET) $(EXAMPLE_PATH)/Examples/contrexPPTA3.imi $(EXAMPLE_PATH)/Examples/contrexPPTA.pi0

##### CASE STUDIES : HARDWARE #####

# 	$(TARGET) $(EXAMPLE_PATH)/AndOr/AndOr.imi -mode reachability -post-limit 10
	$(TARGET) $(EXAMPLE_PATH)/AndOr/AndOr.imi -mode reachability -states-limit 10

# 	$(TARGET) $(EXAMPLE_PATH)/AndOr/AndOr.imi -mode reachability -post-limit 10 -PTA2GML

# 	$(TARGET) $(EXAMPLE_PATH)/AndOr/AndOr.imi $(EXAMPLE_PATH)/AndOr/AndOr.pi0
# 	$(TARGET) $(EXAMPLE_PATH)/AndOr/AndOr.imi $(EXAMPLE_PATH)/AndOr/AndOr.pi0 -no-dot -no-log -statistics 
# 	$(TARGET) $(EXAMPLE_PATH)/AndOr/AndOr.imi $(EXAMPLE_PATH)/AndOr/AndOr.pi0 -no-dot -no-log -statistics -dynamic
# 	$(TARGET) $(EXAMPLE_PATH)/AndOr/AndOr.imi $(EXAMPLE_PATH)/AndOr/AndOr.pi0 -no-dot -no-log -jobshop
# 	bin/IMITATOR2.4 $(EXAMPLE_PATH)/AndOr/AndOr.imi $(EXAMPLE_PATH)/AndOr/AndOr.pi0 -no-dot -no-log -jobshop
# 	bin/IMITATOR2.375 $(EXAMPLE_PATH)/AndOr/AndOr.imi $(EXAMPLE_PATH)/AndOr/AndOr.pi0 -no-dot -no-log -jobshop
# 	bin/IMITATOR2.370 $(EXAMPLE_PATH)/AndOr/AndOr.imi $(EXAMPLE_PATH)/AndOr/AndOr.pi0 -no-dot -no-log -statistics

	# 	$(TARGET) $(EXAMPLE_PATH)/AndOr/AndOr.imi $(EXAMPLE_PATH)/AndOr/AndOr.v0 -mode cover

# 	$(TARGET) $(EXAMPLE_PATH)/Flipflop/flipflop.imi -mode reachability 
# 	$(TARGET) $(EXAMPLE_PATH)/Flipflop/flipflop.imi -mode reachability -no-dot -no-log -jobshop
# 	bin/IMITATOR2.4 $(EXAMPLE_PATH)/Flipflop/flipflop.imi -mode reachability -no-dot -no-log -jobshop
# 	bin/IMITATOR2.375 $(EXAMPLE_PATH)/Flipflop/flipflop.imi -mode reachability -no-dot -no-log -jobshop
# 	bin/IMITATOR2.374 $(EXAMPLE_PATH)/Flipflop/flipflop.imi -mode reachability -no-dot -no-log -jobshop
# 	bin/IMITATOR2.371 $(EXAMPLE_PATH)/Flipflop/flipflop.imi -mode reachability -no-dot -no-log -statistics
# 	bin/IMITATOR2.370 $(EXAMPLE_PATH)/Flipflop/flipflop.imi -mode reachability
# 	bin/IMITATOR2.35.111115 $(EXAMPLE_PATH)/Flipflop/flipflop.imi -mode reachability
# 	bin/IMITATOR2.35 $(EXAMPLE_PATH)/Flipflop/flipflop.imi -mode reachability -no-dot -no-log -statistics
# 	bin/IMITATOR2.34.111115 $(EXAMPLE_PATH)/Flipflop/flipflop.imi -mode reachability -no-dot -no-log 

# 	$(TARGET) $(EXAMPLE_PATH)/Flipflop/flipflop.imi -mode reachability -tree -debug low
# 	$(TARGET) $(EXAMPLE_PATH)/Flipflop/flipflop.imi -mode reachability -acyclic -debug low
# 		$(TARGET) $(EXAMPLE_PATH)/Flipflop/flipflop.imi $(EXAMPLE_PATH)/Flipflop/flipflop.pi0 -no-dot -no-log -jobshop -no-random
# 		bin/IMITATOR2.4 $(EXAMPLE_PATH)/Flipflop/flipflop.imi $(EXAMPLE_PATH)/Flipflop/flipflop.pi0 -no-dot -no-log -jobshop -no-random
# 	$(TARGET) $(EXAMPLE_PATH)/Flipflop/flipflop.imi $(EXAMPLE_PATH)/Flipflop/flipflop.pi0 -no-dot -no-log -statistics -acyclic
# 	$(TARGET) $(EXAMPLE_PATH)/Flipflop/flipflop.imi $(EXAMPLE_PATH)/Flipflop/flipflop.pi0 -no-dot -no-log -statistics -tree
# 	$(TARGET) $(EXAMPLE_PATH)/Flipflop/flipflop.imi $(EXAMPLE_PATH)/Flipflop/flipflop.pi0 -no-dot -no-log -statistics -dynamic
# 	$(TARGET) $(EXAMPLE_PATH)/Flipflop/flipflop.imi $(EXAMPLE_PATH)/Flipflop/flipflop.pi0 -no-dot -no-log -statistics -jobshop
# 	./IMITATOR Examples/Flipflop/flipflop.imi Examples/Flipflop/flipflop.v0 -mode cover -log-prefix Examples/Flipflop/carto_d3_d4/carto_d3_d4
# 	./IMITATOR Examples/Flipflop/flipflop.imi Examples/Flipflop/flipflop.v0 -mode random1000  -log-prefix Examples/Flipflop/test2/test
# 	./IMITATOR Examples/Flipflop/flipflop.imi Examples/Flipflop/flipflop.v0 -mode cover
# 	# 	./IMITATOR Examples/Flipflop/flipflop.imi Examples/Flipflop/flipflop.v0 -mode cover -no-log -time-limit 1 -post-limit 25
# 	./IMITATOR Examples/Flipflop/flipflop.imi Examples/Flipflop/flipflop.v0 -mode cover -no-log -no-dot

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
# 	$(TARGET) $(EXAMPLE_PATH)/Latch/latchValmem.imi $(EXAMPLE_PATH)/Latch/latchValmem.pi0 -no-dot -no-log 
# 	$(TARGET) $(EXAMPLE_PATH)/Latch/latchValmem.imi $(EXAMPLE_PATH)/Latch/latchValmem.pi0 -no-dot -no-log -dynamic
# 	$(TARGET) $(EXAMPLE_PATH)/Latch/latchValmem.imi $(EXAMPLE_PATH)/Latch/latchValmem.pi0 -no-dot -no-log -jobshop

# 	$(TARGET) $(EXAMPLE_PATH)/SRlatch/SRlatch.imi -mode reachability -no-dot -no-log -statistics 
# 	./IMITATOR Examples/SRlatch/SRlatch.imi Examples/SRlatch/SRlatch.pi0
# 	./IMITATOR Examples/SRlatch/SRlatch.imi Examples/SRlatch/SRlatch.v0 -mode cover

# 	./IMITATOR Examples/SRlatch/SRlatch_delais_fixes.imi -mode reachability
# 	./IMITATOR Examples/SRlatch/SRlatch_delais_fixes.imi Examples/SRlatch/SRlatch_delais_fixes.pi0
# 	./IMITATOR Examples/SRlatch/SRlatch_delais_fixes.imi Examples/SRlatch/SRlatch_delais_fixes.v0 -mode cover -no-dot

# 	./IMITATOR Examples/SRlatch/sr_latch.hy -mode reachability
# 	./IMITATOR Examples/SRlatch/sr_latch_nand.hy -mode reachability


##### CASE STUDIES : PROTOCOLS #####

# 	$(TARGET) $(EXAMPLE_PATH)/Train/TrainAHV93.imi -mode reachability -no-dot -no-log 
# 	bin/IMITATOR2.34.111115 $(EXAMPLE_PATH)/Train/TrainAHV93.imi -mode reachability -no-dot -no-log
# 	
# 	$(TARGET) $(EXAMPLE_PATH)/Train/TrainAHV93.imi $(EXAMPLE_PATH)/Train/TrainAHV93.pi0 -incl -no-dot -no-log
# 	bin/IMITATOR2.41 $(EXAMPLE_PATH)/Train/TrainAHV93.imi $(EXAMPLE_PATH)/Train/TrainAHV93.pi0 -incl -no-dot -no-log
# 	bin/IMITATOR2.4 $(EXAMPLE_PATH)/Train/TrainAHV93.imi $(EXAMPLE_PATH)/Train/TrainAHV93.pi0 -incl -no-dot -no-log -jobshop
# 	bin/IMITATOR2.375 $(EXAMPLE_PATH)/Train/TrainAHV93.imi $(EXAMPLE_PATH)/Train/TrainAHV93.pi0 -incl -no-dot -no-log -jobshop
# 	bin/IMITATOR2.374 $(EXAMPLE_PATH)/Train/TrainAHV93.imi $(EXAMPLE_PATH)/Train/TrainAHV93.pi0 -incl -no-dot -no-log -jobshop
# 	bin/IMITATOR2.371 $(EXAMPLE_PATH)/Train/TrainAHV93.imi $(EXAMPLE_PATH)/Train/TrainAHV93.pi0 -incl -no-dot -no-log -statistics
# 	bin/IMITATOR2.370 $(EXAMPLE_PATH)/Train/TrainAHV93.imi $(EXAMPLE_PATH)/Train/TrainAHV93.pi0 -incl -no-dot -no-log -statistics
# 	bin/IMITATOR2.34.111115 $(EXAMPLE_PATH)/Train/TrainAHV93.imi $(EXAMPLE_PATH)/Train/TrainAHV93.pi0 -IMincl -no-dot -no-log -statistics

# 	$(TARGET) $(EXAMPLE_PATH)/BangOlufsen/BangOlufsen.imi -mode reachability 
# 	$(TARGET) $(EXAMPLE_PATH)/BangOlufsen/BangOlufsen.imi $(EXAMPLE_PATH)/BangOlufsen/BangOlufsen.pi0 -post-limit 12 -incl -no-dot -no-log -statistics
# 	$(TARGET) $(EXAMPLE_PATH)/BangOlufsen/BangOlufsen.imi $(EXAMPLE_PATH)/BangOlufsen/BangOlufsen.pi0 -post-limit 12 -incl -no-dot -no-log -statistics -dynamic
# 	$(TARGET) $(EXAMPLE_PATH)/BangOlufsen/BangOlufsen.imi $(EXAMPLE_PATH)/BangOlufsen/BangOlufsen.pi0 -post-limit 12 -incl -no-dot -no-log -statistics -jobshop

# 	./IMITATOR Examples/BangOlufsen/BangOlufsen2.imi -mode reachability -no-dot
# 	./IMITATOR Examples/BangOlufsen/BangOlufsen2.imi -mode reachability -no-dot -post-limit 30 -debug low

# 	$(TARGET) $(EXAMPLE_PATH)/BRP/brp.imi -mode reachability 
# 	$(TARGET) $(EXAMPLE_PATH)/BRP/brp.imi $(EXAMPLE_PATH)/BRP/brp.pi0 -no-dot -no-log -statistics
# 	$(TARGET) $(EXAMPLE_PATH)/BRP/brp.imi $(EXAMPLE_PATH)/BRP/brp.pi0 -no-dot -no-log -statistics -dynamic
# 	$(TARGET) $(EXAMPLE_PATH)/BRP/brp.imi $(EXAMPLE_PATH)/BRP/brp.pi0 -no-dot -no-log
# 	bin/IMITATOR2.41 $(EXAMPLE_PATH)/BRP/brp.imi $(EXAMPLE_PATH)/BRP/brp.pi0 -no-dot -no-log
# 	bin/IMITATOR2.4 $(EXAMPLE_PATH)/BRP/brp.imi $(EXAMPLE_PATH)/BRP/brp.pi0 -no-dot -no-log -jobshop
# 	bin/IMITATOR2.375 $(EXAMPLE_PATH)/BRP/brp.imi $(EXAMPLE_PATH)/BRP/brp.pi0 -no-dot -no-log -jobshop
# 	bin/IMITATOR2.374 $(EXAMPLE_PATH)/BRP/brp.imi $(EXAMPLE_PATH)/BRP/brp.pi0 -no-dot -no-log -jobshop
# 	bin/IMITATOR2.371 $(EXAMPLE_PATH)/BRP/brp.imi $(EXAMPLE_PATH)/BRP/brp.pi0 -no-dot -no-log -statistics
# 	bin/IMITATOR2.370 $(EXAMPLE_PATH)/BRP/brp.imi $(EXAMPLE_PATH)/BRP/brp.pi0 -no-dot -no-log -statistics
# 	bin/IMITATOR2.35 $(EXAMPLE_PATH)/BRP/brp.imi $(EXAMPLE_PATH)/BRP/brp.pi0 -no-dot -no-log -statistics
# 	bin/IMITATOR2.34.111115 $(EXAMPLE_PATH)/BRP/brp.imi $(EXAMPLE_PATH)/BRP/brp.pi0 -no-dot -no-log 
# 	./IMITATOR Examples/BRP/brp.imi Examples/BRP/brp.pi0 -post-limit 10 -time-limit 2
# 	./IMITATOR Examples/BRP/brp.imi Examples/BRP/brp.v0 -mode cover

# 	$(TARGET) $(EXAMPLE_PATH)/RCP/RCP.imi -mode reachability -PTA2GML
# 	$(TARGET) $(EXAMPLE_PATH)/RCP/RCP.imi $(EXAMPLE_PATH)/RCP/RCP.pi0 -no-dot -no-log -statistics
# 	$(TARGET) $(EXAMPLE_PATH)/RCP/RCP.imi $(EXAMPLE_PATH)/RCP/RCP.pi0 -no-dot -no-log -statistics -dynamic
# 	$(TARGET) $(EXAMPLE_PATH)/RCP/RCP.imi $(EXAMPLE_PATH)/RCP/RCP.pi0 -no-dot -no-log
# 	bin/IMITATOR2.41 $(EXAMPLE_PATH)/RCP/RCP.imi $(EXAMPLE_PATH)/RCP/RCP.pi0 -no-dot -no-log
# 	bin/IMITATOR2.4 $(EXAMPLE_PATH)/RCP/RCP.imi $(EXAMPLE_PATH)/RCP/RCP.pi0 -no-dot -no-log -jobshop
# 	bin/IMITATOR2.375 $(EXAMPLE_PATH)/RCP/RCP.imi $(EXAMPLE_PATH)/RCP/RCP.pi0 -no-dot -no-log -jobshop
# 	bin/IMITATOR2.374 $(EXAMPLE_PATH)/RCP/RCP.imi $(EXAMPLE_PATH)/RCP/RCP.pi0 -no-dot -no-log -jobshop 
# 	bin/IMITATOR2.371 $(EXAMPLE_PATH)/RCP/RCP.imi $(EXAMPLE_PATH)/RCP/RCP.pi0 -no-dot -no-log -statistics
# 	bin/IMITATOR2.370 $(EXAMPLE_PATH)/RCP/RCP.imi $(EXAMPLE_PATH)/RCP/RCP.pi0 -no-dot -no-log -statistics
# 	bin/IMITATOR2.35 $(EXAMPLE_PATH)/RCP/RCP.imi $(EXAMPLE_PATH)/RCP/RCP.pi0 -no-dot -no-log -statistics
# 	bin/IMITATOR2.34.111115 $(EXAMPLE_PATH)/RCP/RCP.imi $(EXAMPLE_PATH)/RCP/RCP.pi0 -no-dot -no-log
# 	./IMITATOR Examples/RCP/RCP.imi Examples/RCP/RCP.v0 -mode cover -no-dot -log-prefix Examples/RCP/temp/RCP -no-log

# 	./IMITATOR Examples/RCP/RCP_bounded.imi Examples/RCP/RCP_bounded.pi0 -no-dot -no-log

# 	$(TARGET) $(EXAMPLE_PATH)/CSMACD/csmacdPrism.imi -mode reachability 
# 	$(TARGET) $(EXAMPLE_PATH)/CSMACD/csmacdPrism.imi $(EXAMPLE_PATH)/CSMACD/csmacdPrism.pi0 -no-dot -no-log
# 	$(TARGET) $(EXAMPLE_PATH)/CSMACD/csmacdPrism.imi $(EXAMPLE_PATH)/CSMACD/csmacdPrism.pi0 -no-dot -no-log -statistics -dynamic
# 	$(TARGET) $(EXAMPLE_PATH)/CSMACD/csmacdPrism.imi $(EXAMPLE_PATH)/CSMACD/csmacdPrism.pi0 -no-dot -no-log -statistics -jobshop
# 	$(TARGET) $(EXAMPLE_PATH)/CSMACD/csmacdPrism.imi $(EXAMPLE_PATH)/CSMACD/csmacdPrism.pi0 -no-dot -no-log
# 	bin/IMITATOR2.41 $(EXAMPLE_PATH)/CSMACD/csmacdPrism.imi $(EXAMPLE_PATH)/CSMACD/csmacdPrism.pi0 -no-dot -no-log
# 	bin/IMITATOR2.4 $(EXAMPLE_PATH)/CSMACD/csmacdPrism.imi $(EXAMPLE_PATH)/CSMACD/csmacdPrism.pi0 -no-dot -no-log -jobshop
# 	bin/IMITATOR2.375 $(EXAMPLE_PATH)/CSMACD/csmacdPrism.imi $(EXAMPLE_PATH)/CSMACD/csmacdPrism.pi0 -no-dot -no-log -jobshop
# 	bin/IMITATOR2.374 $(EXAMPLE_PATH)/CSMACD/csmacdPrism.imi $(EXAMPLE_PATH)/CSMACD/csmacdPrism.pi0 -no-dot -no-log -jobshop
# 	bin/IMITATOR2.370 $(EXAMPLE_PATH)/CSMACD/csmacdPrism.imi $(EXAMPLE_PATH)/CSMACD/csmacdPrism.pi0 -no-dot -no-log -statistics

# 	$(TARGET) $(EXAMPLE_PATH)/CSMACD/csmacdPrism_with_renamed_actions.imi -mode reachability -no-merging
# 	$(TARGET) $(EXAMPLE_PATH)/CSMACD/csmacdPrism_with_renamed_actions.imi $(EXAMPLE_PATH)/CSMACD/csmacdPrism.pi0

# 	./IMITATOR Examples/CSMACD/csmacdPrism_2p.imi -mode reachability
# 	./IMITATOR Examples/CSMACD/csmacdPrism_2p.imi Examples/CSMACD/csmacdPrism_2p.pi0 
# 	./IMITATOR Examples/CSMACD/csmacdPrism_2p.imi Examples/CSMACD/csmacdPrism_2p.v0 -mode cover -log-prefix Examples/CSMACD/carto_2p/csmacdPrism_2p

# 	$(TARGET) $(EXAMPLE_PATH)/Wlan/wlan.imi -mode reachability
# 	$(TARGET) $(EXAMPLE_PATH)/Wlan/wlan.imi $(EXAMPLE_PATH)/Wlan/wlan.pi0 -no-dot -no-log -statistics -jobshop
# 	$(TARGET) $(EXAMPLE_PATH)/Wlan/wlan.imi $(EXAMPLE_PATH)/Wlan/wlan.pi0 -post-limit 10 -no-dot -no-log -statistics 
# 	$(TARGET) $(EXAMPLE_PATH)/Wlan/wlan.imi $(EXAMPLE_PATH)/Wlan/wlan.pi0 -post-limit 10 -no-dot -no-log -statistics -dynamic
# 	$(TARGET) $(EXAMPLE_PATH)/Wlan/wlan.imi $(EXAMPLE_PATH)/Wlan/wlan.pi0 -post-limit 10 -no-dot -no-log
# 	bin/IMITATOR2.41 $(EXAMPLE_PATH)/Wlan/wlan.imi $(EXAMPLE_PATH)/Wlan/wlan.pi0 -post-limit 10 -no-dot -no-log
# 	bin/IMITATOR2.4 $(EXAMPLE_PATH)/Wlan/wlan.imi $(EXAMPLE_PATH)/Wlan/wlan.pi0 -post-limit 10 -no-dot -no-log -jobshop
# 	bin/IMITATOR2.375 $(EXAMPLE_PATH)/Wlan/wlan.imi $(EXAMPLE_PATH)/Wlan/wlan.pi0 -post-limit 10 -no-dot -no-log -jobshop
# 	bin/IMITATOR2.374 $(EXAMPLE_PATH)/Wlan/wlan.imi $(EXAMPLE_PATH)/Wlan/wlan.pi0 -post-limit 10 -no-dot -no-log -jobshop
# 	bin/IMITATOR2.371 $(EXAMPLE_PATH)/Wlan/wlan.imi $(EXAMPLE_PATH)/Wlan/wlan.pi0 -post-limit 10 -no-dot -no-log -statistics
# 	bin/IMITATOR2.370 $(EXAMPLE_PATH)/Wlan/wlan.imi $(EXAMPLE_PATH)/Wlan/wlan.pi0 -post-limit 10 -no-dot -no-log -statistics
# 	bin/IMITATOR2.35 $(EXAMPLE_PATH)/Wlan/wlan.imi $(EXAMPLE_PATH)/Wlan/wlan.pi0 -post-limit 10 -no-dot -no-log -statistics
# 	bin/IMITATOR2.34.111115 $(EXAMPLE_PATH)/Wlan/wlan.imi $(EXAMPLE_PATH)/Wlan/wlan.pi0 -post-limit 10 -no-dot -no-log

# 	$(TARGET) $(EXAMPLE_PATH)/Wlan/wlan2_without_asap.imi $(EXAMPLE_PATH)/Wlan/wlan2_without_asap.pi0
# 	$(TARGET) $(EXAMPLE_PATH)/Wlan/wlan2_without_asap_without_minmax.imi $(EXAMPLE_PATH)/Wlan/wlan2_without_asap_without_minmax.pi0 -no-dot -no-log

# 	./IMITATOR Examples/Wlan/wlan_boff2.imi Examples/Wlan/wlan_boff2.pi0 -timed

# 	./IMITATOR Examples/Wlan/wlan2_for_im2.imi -mode reachability -debug low
# 	./IMITATOR Examples/Wlan/wlan2_for_im2.imi Examples/Wlan/wlan2_for_im2.pi0 -timed

##### CASE STUDIES : VALMEM #####

# 	./IMITATOR Examples/Valmem/spsmall.imi -mode reachability -timed -time-limit 60
# -debug medium -inclusion -post-limit 2 -sync-auto-detect
# 	$(TARGET) $(EXAMPLE_PATH)/Valmem/spsmall.imi $(EXAMPLE_PATH)/Valmem/spsmall.pi0 -no-dot -no-log -PTA2GML
# 	$(TARGET) $(EXAMPLE_PATH)/Valmem/spsmall.imi $(EXAMPLE_PATH)/Valmem/spsmall.pi0 -no-dot -no-log -statistics
# 	$(TARGET) $(EXAMPLE_PATH)/Valmem/spsmall.imi $(EXAMPLE_PATH)/Valmem/spsmall.pi0 -no-dot -no-log -statistics -acyclic
# 	$(TARGET) $(EXAMPLE_PATH)/Valmem/spsmall.imi $(EXAMPLE_PATH)/Valmem/spsmall.pi0 -no-dot -no-log -statistics -tree -acyclic
# 	$(TARGET) $(EXAMPLE_PATH)/Valmem/spsmall.imi $(EXAMPLE_PATH)/Valmem/spsmall.pi0 -no-dot -no-log -statistics -dynamic
# 	$(TARGET) $(EXAMPLE_PATH)/Valmem/spsmall.imi $(EXAMPLE_PATH)/Valmem/spsmall.pi0 -no-dot -no-log
# 	bin/IMITATOR2.41 $(EXAMPLE_PATH)/Valmem/spsmall.imi $(EXAMPLE_PATH)/Valmem/spsmall.pi0 -no-dot -no-log
# 	bin/IMITATOR2.4 $(EXAMPLE_PATH)/Valmem/spsmall.imi $(EXAMPLE_PATH)/Valmem/spsmall.pi0 -no-dot -no-log -jobshop
# 	bin/IMITATOR2.375 $(EXAMPLE_PATH)/Valmem/spsmall.imi $(EXAMPLE_PATH)/Valmem/spsmall.pi0 -no-dot -no-log -jobshop
# 	bin/IMITATOR2.374 $(EXAMPLE_PATH)/Valmem/spsmall.imi $(EXAMPLE_PATH)/Valmem/spsmall.pi0 -no-dot -no-log -jobshop
# 	bin/IMITATOR2.371 $(EXAMPLE_PATH)/Valmem/spsmall.imi $(EXAMPLE_PATH)/Valmem/spsmall.pi0 -no-dot -no-log -statistics
# 	bin/IMITATOR2.370 $(EXAMPLE_PATH)/Valmem/spsmall.imi $(EXAMPLE_PATH)/Valmem/spsmall.pi0 -no-dot -no-log -statistics
# 	bin/IMITATOR2.35 $(EXAMPLE_PATH)/Valmem/spsmall.imi $(EXAMPLE_PATH)/Valmem/spsmall.pi0 -no-dot -no-log -statistics
# 	bin/IMITATOR2.34.111115 $(EXAMPLE_PATH)/Valmem/spsmall.imi $(EXAMPLE_PATH)/Valmem/spsmall.pi0 -no-dot -no-log
# 	./IMITATOR Examples/Valmem/spsmall.imi Examples/Valmem/spsmall.v0 -mode cover -no-dot -no-log

# 	./IMITATOR Examples/Valmem/spsmall_obs.imi -mode reachability -with-parametric-log
# 	./IMITATOR Examples/Valmem/spsmall_obs.imi Examples/Valmem/spsmall.pi0
# 	./IMITATOR Examples/Valmem/spsmall_obs.imi Examples/Valmem/spsmall.v0 -mode cover -log-prefix Examples/Valmem/carto/spsmall

# 	$(TARGET) $(EXAMPLE_PATH)/Valmem/LSV.imi $(EXAMPLE_PATH)/Valmem/delais1_hy.pi0 -no-dot -no-log -statistics 
# 	$(TARGET) $(EXAMPLE_PATH)/Valmem/LSV.imi $(EXAMPLE_PATH)/Valmem/delais1_hy.pi0 -no-dot -no-log -post-limit 31 -jobshop -debug medium
# 	$(TARGET) $(EXAMPLE_PATH)/Valmem/LSV.imi $(EXAMPLE_PATH)/Valmem/delais1_hy.pi0 -no-dot -no-log -statistics -acyclic -dynamic
# 	bin/IMITATOR2.34.111115 $(EXAMPLE_PATH)/Valmem/LSV.imi $(EXAMPLE_PATH)/Valmem/delais1_hy.pi0 -no-dot -no-log -dynamic
# 	$(TARGET) $(EXAMPLE_PATH)/Valmem/LSV.imi $(EXAMPLE_PATH)/Valmem/delais1_hy.pi0 -no-dot -no-log -post-limit 13 -no-random
# 	bin/IMITATOR2.41 $(EXAMPLE_PATH)/Valmem/LSV.imi $(EXAMPLE_PATH)/Valmem/delais1_hy.pi0 -no-dot -no-log -post-limit 13 -no-random
# 	bin/IMITATOR2.4 $(EXAMPLE_PATH)/Valmem/LSV.imi $(EXAMPLE_PATH)/Valmem/delais1_hy.pi0 -no-dot -no-log -post-limit 13 -no-random -jobshop
# 	bin/IMITATOR2.375 $(EXAMPLE_PATH)/Valmem/LSV.imi $(EXAMPLE_PATH)/Valmem/delais1_hy.pi0 -no-dot -no-log -post-limit 13 -no-random -jobshop
# 	bin/IMITATOR2.374 $(EXAMPLE_PATH)/Valmem/LSV.imi $(EXAMPLE_PATH)/Valmem/delais1_hy.pi0 -no-dot -no-log -post-limit 13 -jobshop
# 	bin/IMITATOR2.371 $(EXAMPLE_PATH)/Valmem/LSV.imi $(EXAMPLE_PATH)/Valmem/delais1_hy.pi0 -no-dot -no-log -statistics -post-limit 13
# 	bin/IMITATOR2.370 $(EXAMPLE_PATH)/Valmem/LSV.imi $(EXAMPLE_PATH)/Valmem/delais1_hy.pi0 -no-dot -no-log -statistics -post-limit 13
# 	bin/IMITATOR2.36 $(EXAMPLE_PATH)/Valmem/LSV.imi $(EXAMPLE_PATH)/Valmem/delais1_hy.pi0 -no-dot -no-log -statistics -post-limit 13
# 	bin/IMITATOR2.35 $(EXAMPLE_PATH)/Valmem/LSV.imi $(EXAMPLE_PATH)/Valmem/delais1_hy.pi0 -no-dot -no-log -statistics -post-limit 13
# 	bin/IMITATOR2.34.111115 $(EXAMPLE_PATH)/Valmem/LSV.imi $(EXAMPLE_PATH)/Valmem/delais1_hy.pi0 -no-dot -no-log -post-limit 13
# 	$(TARGET) $(EXAMPLE_PATH)/Valmem/sp_1x2_md_no.imi Examples/Valmem/sp_1x2_md_no.pi0 

##### CASE STUDIES : SIMOP #####
# 	$(TARGET) $(EXAMPLE_PATH)/SIMOP/simop.imi -mode reachability 

# 	$(TARGET) $(EXAMPLE_PATH)/SIMOP/simop.imi $(EXAMPLE_PATH)/SIMOP/simop.pi0 -incl -no-dot -no-log -statistics
# 	$(TARGET) $(EXAMPLE_PATH)/SIMOP/simop.imi $(EXAMPLE_PATH)/SIMOP/simop.pi0 -incl -no-dot -no-log -statistics -dynamic
# 	$(TARGET) $(EXAMPLE_PATH)/SIMOP/simop.imi $(EXAMPLE_PATH)/SIMOP/simop.pi0 -incl -no-dot -no-log
# 	bin/IMITATOR2.41 $(EXAMPLE_PATH)/SIMOP/simop.imi $(EXAMPLE_PATH)/SIMOP/simop.pi0 -incl -no-dot -no-log
# 	bin/IMITATOR2.4 $(EXAMPLE_PATH)/SIMOP/simop.imi $(EXAMPLE_PATH)/SIMOP/simop.pi0 -incl -no-dot -no-log -jobshop
# 	bin/IMITATOR2.375 $(EXAMPLE_PATH)/SIMOP/simop.imi $(EXAMPLE_PATH)/SIMOP/simop.pi0 -incl -no-dot -no-log -jobshop
# 	bin/IMITATOR2.374 $(EXAMPLE_PATH)/SIMOP/simop.imi $(EXAMPLE_PATH)/SIMOP/simop.pi0 -incl -no-dot -no-log -jobshop
# 	bin/IMITATORsafeJS $(EXAMPLE_PATH)/SIMOP/simop.imi $(EXAMPLE_PATH)/SIMOP/simop.pi0 -incl -no-dot -no-log -jobshop -statistics
# 	bin/IMITATOR2.370 $(EXAMPLE_PATH)/SIMOP/simop.imi $(EXAMPLE_PATH)/SIMOP/simop.pi0 -incl -no-dot -no-log -statistics
# 	bin/IMITATOR2.36 $(EXAMPLE_PATH)/SIMOP/simop.imi $(EXAMPLE_PATH)/SIMOP/simop.pi0 -incl -no-dot -no-log -statistics
# 	bin/IMITATOR2.35 $(EXAMPLE_PATH)/SIMOP/simop.imi $(EXAMPLE_PATH)/SIMOP/simop.pi0 -incl -no-dot -no-log -statistics
# 	bin/IMITATOR2.34.111115 $(EXAMPLE_PATH)/SIMOP/simop.imi $(EXAMPLE_PATH)/SIMOP/simop.pi0 -IMincl -no-dot -no-log


##### SCHEDULING #####
# 	$(TARGET) $(EXAMPLE_PATH)/Scheduling/am02.imi -mode reachability 
# 	$(TARGET) $(EXAMPLE_PATH)/Scheduling/am02.imi $(EXAMPLE_PATH)/Scheduling/am02.pi0
# 	$(TARGET) $(EXAMPLE_PATH)/Scheduling/LA02_2.imi $(EXAMPLE_PATH)/Scheduling/LA02_2.pi0 -incl
# 	bin/IMITATOR2.41 $(EXAMPLE_PATH)/Scheduling/LA02_2.imi $(EXAMPLE_PATH)/Scheduling/LA02_2.pi0 -incl
# 	bin/IMITATOR2.4 $(EXAMPLE_PATH)/Scheduling/LA02_2.imi $(EXAMPLE_PATH)/Scheduling/LA02_2.pi0 -incl -jobshop

##### JOB SHOP #####
# 	$(TARGET) $(EXAMPLE_PATH)/Jobshop/maler_2_4.imi -mode reachability -no-dot -no-log -incl 
# 	bin/IMITATOR2.4 $(EXAMPLE_PATH)/Jobshop/maler_2_4.imi -mode reachability -no-dot -no-log -incl -jobshop
# 	bin/IMITATOR2.375 $(EXAMPLE_PATH)/Jobshop/maler_2_4.imi -mode reachability -no-dot -no-log -incl -jobshop
# 	bin/IMITATOR2.374 $(EXAMPLE_PATH)/Jobshop/maler_2_4.imi -mode reachability -no-dot -no-log -incl -jobshop
# 	bin/IMITATOR2.371 $(EXAMPLE_PATH)/Jobshop/maler_2_4.imi -mode reachability -no-dot -no-log -incl -statistics
# 	bin/IMITATOR2.370 $(EXAMPLE_PATH)/Jobshop/maler_2_4.imi -mode reachability -no-dot -no-log -incl -statistics
# 	bin/IMITATOR2.36 $(EXAMPLE_PATH)/Jobshop/maler_2_4.ancien.imi -mode reachability -no-dot -no-log -incl -statistics
# 	bin/IMITATOR2.35 $(EXAMPLE_PATH)/Jobshop/maler_2_4.imi -mode reachability -no-dot -no-log -incl -statistics
# 	bin/IMITATOR2.35.111115 $(EXAMPLE_PATH)/Jobshop/maler_2_4.imi -mode reachability -no-dot -no-log -IMincl
# 	bin/IMITATOR2.34.111115 $(EXAMPLE_PATH)/Jobshop/maler_2_4.ancien.imi -mode reachability -no-dot -no-log -IMincl

# 	$(TARGET) $(EXAMPLE_PATH)/Jobshop/maler_2_4.imi $(EXAMPLE_PATH)/Jobshop/maler_2_4.pi0 -no-dot -no-log -acyclic -statistics
# 	$(TARGET) $(EXAMPLE_PATH)/Jobshop/maler_2_4.imi $(EXAMPLE_PATH)/Jobshop/maler_2_4.pi0 -no-dot -no-log -incl
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
# 	bin/IMITATOR2.35 $(EXAMPLE_PATH)/Jobshop/maler_3_4.imi -mode reachability -no-dot -no-log -incl -statistics -post-limit 10
# 	bin/IMITATOR2.34.111115 $(EXAMPLE_PATH)/Jobshop/maler_3_4_inst.imi -mode reachability -no-dot -no-log -IMincl

# 	$(TARGET) $(EXAMPLE_PATH)/Jobshop/maler_3_4.imi $(EXAMPLE_PATH)/Jobshop/maler_3_4.pi0 -no-dot -no-log -incl
# 	bin/IMITATOR2.41 $(EXAMPLE_PATH)/Jobshop/maler_3_4.imi $(EXAMPLE_PATH)/Jobshop/maler_3_4.pi0 -no-dot -no-log -incl
# 	bin/IMITATOR2.4 $(EXAMPLE_PATH)/Jobshop/maler_3_4.imi $(EXAMPLE_PATH)/Jobshop/maler_3_4.pi0 -no-dot -no-log -incl -jobshop
# 	bin/IMITATOR2.375 $(EXAMPLE_PATH)/Jobshop/maler_3_4.imi $(EXAMPLE_PATH)/Jobshop/maler_3_4.pi0 -no-dot -no-log -incl -jobshop
# 	bin/IMITATOR2.371 $(EXAMPLE_PATH)/Jobshop/maler_3_4.imi $(EXAMPLE_PATH)/Jobshop/maler_3_4.pi0 -no-dot -no-log -incl -statistics -post-limit 9

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
# 	$(TARGET) $(EXAMPLE_PATH)/Jobshop/maler_4_4.imi -mode reachability -no-dot -no-log -incl -acyclic -post-limit 10
# 	bin/IMITATOR2.375 $(EXAMPLE_PATH)/Jobshop/maler_4_4.imi -mode reachability -no-dot -no-log -incl -acyclic -post-limit 10
# 	bin/IMITATOR2.370 $(EXAMPLE_PATH)/Jobshop/maler_4_4.imi -mode reachability -no-dot -no-log -incl -statistics -post-limit 8
# 	bin/IMITATOR2.36 $(EXAMPLE_PATH)/Jobshop/maler_4_4.imi -mode reachability -no-dot -no-log -incl -statistics


##### ANCIEN #####

# 	./IMITATOR Examples/test.imi Examples/test.pi0 -debug total
# 	./IMITATOR Examples/testBoucle.imi Examples/vide.pi0 -debug high
# 	./IMITATOR Examples/testPost.imi Examples/testPost.pi0 -debug total
# 	./IMITATOR Examples/testPost.imi Examples/testPost.pi0 -debug total -sync-auto-detect
# 	./IMITATOR Examples/testPost2.imi Examples/vide.pi0 -debug nodebug -timed


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
