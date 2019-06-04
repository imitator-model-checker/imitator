#!/bin/sh

#################################################################
 #
 #                       BUILDATOR
 #
 # Script to build the non-distributed version of IMITATOR
 #
 # Université Paris 13, LIPN, CNRS, France
 #
 # Author:        Étienne André
 #
 # Created:       2014/01/13
 # Last modified: 2019/06/04
 #
################################################################

if [ -f "setup.ml" ]
then
  ocaml setup.ml -distclean
fi

if [ ! -d "bin" ]
then
  mkdir bin
fi

python gen_oasis.py \
&& oasis setup \
&& ocaml setup.ml -configure --enable-tests \
&& ocaml setup.ml -all
