#!/bin/sh

#################################################################
 #
 #                       BUILDUNITTESTATOR
 # 
 # Script to build and execute UNITTESTATOR
 #
 # Universite Paris 13, Sorbonne Paris Cite, LIPN (France)
 # 
 # Author:        Etienne Andre
 # 
 # Created:       2016/04/30
 # Last modified: 2016/04/30
 #
################################################################

if [ -f "setup.ml" ]
then
  ocaml setup.ml -distclean
fi

# 1. Copy config file into _oasis
# 2. Generate setup
# 3. Execute tests
cp oasis-unittestator-config _oasis \
&& oasis setup \
&& ocaml setup.ml -configure --enable-tests \
&& ocaml setup.ml -all \
&& ./bin/UnitTestator
