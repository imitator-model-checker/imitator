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

# options handling
flags="--disable-dev"
while test $# -gt 0; do
  case "$1" in
    --no-distclean)
      disableDistClean=true;;
    --dev)
      disableDistClean=true
      flags="--enable-dev";;
  esac
  shift
done

# clean the project
if [ -f "setup.ml" ] && [ "$disableDistClean" != true ]
then
  ocaml setup.ml -distclean
fi

if [ ! -d "bin" ]
then
  mkdir bin
fi

python gen_oasis.py \
&& oasis setup \
&& ocaml setup.ml -configure --enable-tests $flags \
&& ocaml setup.ml -all
