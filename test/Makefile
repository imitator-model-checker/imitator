IMITATOR = ../bin/IMITATOR
#IMITATOR = `which IMITATOR`

# ========================================================================
# test cases for regression tests
# inverse method:
INV_TESTS = AndOr flipflop SRlatch brp
INV_IMI_FILES = $(addprefix data/, $(addsuffix .imi, $(INV_TESTS)))
INV_PI0_FILES = $(addprefix data/, $(addsuffix .pi0, $(INV_TESTS)))
INV_RESULTS   = $(addprefix log/, $(addsuffix .inv.log, $(INV_TESTS)))

# cartography:
COV_TESTS = AndOr flipflop SRlatch 
COV_IMI_FILES = $(addprefix data/, $(addsuffix .imi, $(COV_TESTS)))
COV_V0_FILES  = $(addprefix data/, $(addsuffix .v0, $(COV_TESTS)))
COV_RESULTS   = $(addprefix log/, $(addsuffix .cov.log, $(COV_TESTS)))


default all: clean regression

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
# rules for checking success

check_termination: regr_inv regr_cov
	@for i in $(INV_RESULTS) ; do \
		echo [CHECK] checking log file "$${i}" ; \
	done
	
clean:
	@ rm -f fail
	@ rm -f $(INV_RESULTS) $(COV_RESULTS)
	@ rm -f data/*.jpg data/*.states
