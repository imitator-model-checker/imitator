#!/bin/sh

#################################################################
 #
 #                       BUILDATOR
 # 
 # Script to build the non-distributed version of IMITATOR
 #
 # Universite Paris 13, Sorbonne Paris Cite, LIPN (France)
 # 
 # Author:        Etienne Andre
 # 
 # Created:       2014/08/18
 # Last modified: 2014/08/18
 #
################################################################

if [ -f "setup.ml" ]
then
  ocaml setup.ml -distclean
fi

python gen_imitator_nondistr.py \
&& python gen_oasis_nondistr.py \
&& oasis setup \
&& ocaml setup.ml -configure --enable-tests \
&& ocaml setup.ml -all

