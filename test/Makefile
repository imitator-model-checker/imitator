IMITATOR = ../bin/IMITATOR
#IMITATOR = `which IMITATOR`

# ========================================================================
# test cases for regression tests
# inverse method:
INV_TESTS = AndOr flipflop SRlatch 
INV_IMI_FILES = $(addprefix data/, $(addsuffix .imi, $(INV_TESTS)))
INV_PI0_FILES = $(addprefix data/, $(addsuffix .pi0, $(INV_TESTS)))
INV_RESULTS   = $(addprefix log/, $(addsuffix .inv.log, $(INV_TESTS)))

# cartography:
COV_TESTS = AndOr flipflop SRlatch 
COV_IMI_FILES = $(addprefix data/, $(addsuffix .imi, $(COV_TESTS)))
COV_V0_FILES  = $(addprefix data/, $(addsuffix .v0, $(COV_TESTS)))
COV_RESULTS   = $(addprefix log/, $(addsuffix .cov.log, $(COV_TESTS)))


default all: clean_regr unit regression

regression: regr_inv regr_cov
	@ echo '================================================'
	@ if ( test -e fail ); then \
	    echo [SUMM] There were failing test cases. ; fi
	@ if ( ! test -e fail ); then \
	    echo [SUMM] All tests passed. ; fi 


# ========================================================================
# rules for regression tests

regr_inv: $(INV_RESULTS)

regr_cov: $(COV_RESULTS)


log/%.inv.log: data/%.imi data/%.pi0
	@ echo [TEST] Testing inverse method for \'$*\' 
	@ $(IMITATOR) data/$*.imi data/$*.pi0 -mode inversemethod > $@
	@ if ( ! fgrep -s "IMITATOR II successfully terminated" $@ > /dev/null ); then \
	  echo [FAIL] Test did not terminate successfully. Check log file \'$@\' ; \
	  touch fail; fi	  

log/%.cov.log: data/%.imi data/%.v0
	@ echo [TEST] Testing cartography for \'$*\' 
	@ $(IMITATOR) data/$*.imi data/$*.v0 -mode cover > $@
	@ if ( ! fgrep -s "IMITATOR II successfully terminated" $@ > /dev/null ); then \
	  echo [FAIL] Test did not terminate successfully. Check log file \'$@\' ; \
	  touch fail; fi

# ========================================================================
# unit tests

OUNIT_PATH = /import/kuehne/local/ounit
APRON_PATH = /import/kuehne/local/apron/lib
EXTLIB_PATH = /usr/lib/ocaml/extlib
INCLUDE = -I src -I ../src -I $(OUNIT_PATH) -I $(EXTLIB_PATH) -I $(APRON_PATH)
LIBS = str.cma unix.cma extLib.cma bigarray.cma gmp.cma apron.cma polkaMPQ.cma oUnit.cma 
IMI_OBJ = Global.cmo NumConst.cmo Automaton.cmo 

UNIT_TESTS = ImitatorParser Automaton NumConst
UNIT_SRC = $(addprefix src/test, $(addsuffix .ml, $(UNIT_TESTS))) 
UNIT_CMI = $(addprefix src/test, $(addsuffix .cmi, $(UNIT_TESTS))) testSuite.cmi
UNIT_OBJ = $(addprefix src/test, $(addsuffix .cmo, $(UNIT_TESTS))) testSuite.cmo

unit: testsuite	
	./testsuite	

testsuite: $(UNIT_OBJ)
	@ echo [LINK] testsuite
	@ ocamlc -o testsuite $(INCLUDE) $(LIBS) $(IMI_OBJ) $(UNIT_OBJ)  

testSuite.cmo: $(UNIT_SRC)

%.cmo: %.ml
	@ echo [OCAMLC] $<
	@ ocamlc -c $(INCLUDE) $<

# ========================================================================	

clean_regr:	
	@ rm -f fail
	@ rm -f $(INV_RESULTS) $(COV_RESULTS)
	@ rm -f data/*.jpg data/*.states

clean: clean_regr
	@ rm -f $(UNIT_OBJ) $(UNIT_CMI)
	@ rm -f testsuite
