#!/bin/sh

#################################################################
 #
 #                       BUILDATOR
 # 
 # Script to build IMITATOR
 #
 # Universite Paris 13, Sorbonne Paris Cite, LIPN (France)
 # 
 # Author:        Etienne Andre
 # 
 # Created:       2014/01/13
 # Last modified: 2014/08/18
 #
################################################################

python gen_oasis.py \
&& oasis setup \
&& ocaml setup.ml -configure --enable-tests \
&& ocaml setup.ml -all

