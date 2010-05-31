IMITATOR = ../bin/IMITATOR
#IMITATOR = `which IMITATOR`

IMITATOR_MISC_OPTIONS = -no-dot -no-log

LINE_DELIM = '-------------------------------------------------'

# ========================================================================
# test cases for regression tests
# reachability
REA_TESTS = flipflop 
REA_IMI_FILES = $(addprefix data/, $(addsuffix .imi, $(REA_TESTS)))
REA_RESULTS   = $(addprefix log/, $(addsuffix .rea.log, $(REA_TESTS)))

# inverse method:
INV_TESTS = AndOr flipflop SRlatch spsmall latchValmem RCP csmacdPrism
INV_IMI_FILES = $(addprefix data/, $(addsuffix .imi, $(INV_TESTS)))
INV_PI0_FILES = $(addprefix data/, $(addsuffix .pi0, $(INV_TESTS)))
INV_RESULTS   = $(addprefix log/, $(addsuffix .inv.log, $(INV_TESTS)))

# cartography:
COV_TESTS = AndOr flipflop SRlatch 
COV_IMI_FILES = $(addprefix data/, $(addsuffix .imi, $(COV_TESTS)))
COV_V0_FILES  = $(addprefix data/, $(addsuffix .v0, $(COV_TESTS)))
COV_RESULTS   = $(addprefix log/, $(addsuffix .cov.log, $(COV_TESTS)))


default all: unit regression

regression: clean_regr regr_rea regr_inv regr_cov	
	@ if ( test -e fail ); then \
	    echo [SUMM] There were failing test cases. ; fi
	@ if ( ! test -e fail ); then \
	    echo [SUMM] All tests passed. ; fi 


# ========================================================================
# rules for regression tests

regr_rea: $(REA_RESULTS)
	@ echo $(LINE_DELIM)
	
regr_inv: $(INV_RESULTS)
	@ echo $(LINE_DELIM)

regr_cov: $(COV_RESULTS)
	@ echo $(LINE_DELIM)

log/%.rea.log: data/%.imi
	@ echo [TEST] Testing reachability analysis for \'$*\' 
	@ $(IMITATOR) data/$*.imi -mode reachability $(IMITATOR_MISC_OPTIONS) > $@
	@ if ( ! fgrep -s "IMITATOR II successfully terminated" $@ > /dev/null ); then \
	  echo [FAIL] Test did not terminate successfully. Check log file \'$@\' ; \
	  touch fail; fi	  

log/%.inv.log: data/%.imi data/%.pi0
	@ echo [TEST] Testing inverse method for \'$*\' 
	@ $(IMITATOR) data/$*.imi data/$*.pi0 -mode inversemethod $(IMITATOR_MISC_OPTIONS) > $@
	@ if ( ! fgrep -s "IMITATOR II successfully terminated" $@ > /dev/null ); then \
	  echo [FAIL] Test did not terminate successfully. Check log file \'$@\' ; \
	  touch fail; fi	  

log/%.cov.log: data/%.imi data/%.v0
	@ echo [TEST] Testing cartography for \'$*\' 
	@ $(IMITATOR) data/$*.imi data/$*.v0 -mode cover $(IMITATOR_MISC_OPTIONS) > $@
	@ if ( ! fgrep -s "IMITATOR II successfully terminated" $@ > /dev/null ); then \
	  echo [FAIL] Test did not terminate successfully. Check log file \'$@\' ; \
	  touch fail; fi

# ========================================================================
# unit tests

# check if paths are inherited from environment or parent makefile
ifndef EXTLIB_PATH
  EXTLIB_PATH = /usr/lib/ocaml/extlib
endif
ifndef OCAML_PPL_PATH
  OCAML_PPL_PATH = $(HOME)/local/lib/ppl
endif 
ifndef OCAML_GMP_PATH
  OCAML_GMP_PATH = $(HOME)/local/lib
endif
ifndef OUNIT_PATH
  OUNIT_PATH = $(HOME)/local/ounit
endif
ifndef LIBS
  LIBS = -cclib -lpwl -cclib -lm -cclib -lgmpxx -cclib -lgmp -cclib -lppl \
     str.cma unix.cma extLib.cma bigarray.cma gmp.cma ppl_ocaml.cma
endif
 
INCLUDE = -I src -I ../src -I ../lib -I $(OUNIT_PATH) -I $(EXTLIB_PATH) -I $(OCAML_PPL_PATH) -I $(OCAML_GMP_PATH)
LIBS += oUnit.cma imitator.cma 
 
UNIT_TESTS = ImitatorParser Automaton NumConst LinearConstraint
UNIT_SRC = $(addprefix src/test, $(addsuffix .ml, $(UNIT_TESTS))) 
UNIT_CMI = $(addprefix src/test, $(addsuffix .cmi, $(UNIT_TESTS))) testSuite.cmi
UNIT_OBJ = $(addprefix src/test, $(addsuffix .cmo, $(UNIT_TESTS))) testSuite.cmo

unit: testsuite	
	@ ./testsuite
	@ echo
	@ echo $(LINE_DELIM)

testsuite: $(UNIT_OBJ)
	@ echo [LINK] testsuite
	@ ocamlc -o testsuite $(INCLUDE) $(LIBS) $(UNIT_OBJ)  

testSuite.cmo: $(UNIT_SRC)

%.cmo: %.ml
	@ echo [OCAMLC] $<
	@ ocamlc -c $(INCLUDE) $<

.depend:
	@ echo [OCAMLDEP]
	@ ocamldep -I ../src -I src src/*.ml > .depend

# ========================================================================	

clean_regr:	
	@ rm -f fail
	@ rm -f $(REA_RESULTS) $(INV_RESULTS) $(COV_RESULTS)
	@ rm -f data/*.jpg data/*.states

clean: clean_regr
	@ rm -f $(UNIT_OBJ) $(UNIT_CMI)	
	@ rm -f testsuite
	@ rm -f .depend

include .depend
