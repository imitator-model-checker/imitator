#!/bin/sh

#################################################################
 #
 #                       BUILDATOR
 # 
 # Script to build the distributed version of IMITATOR
 #
 # Université Paris 13, LIPN, CNRS, France
 # 
 # Author:        Étienne André
 # 
 # Created:       2014/08/18
 # Last modified: 2019/06/04
 #
################################################################

if [ -f "setup.ml" ]
then
  ocaml setup.ml -distclean
fi

python gen_imitator_distr.py \
&& python gen_oasis_distr.py \
&& oasis setup \
&& ocaml setup.ml -configure --enable-tests \
&& ocaml setup.ml -all

