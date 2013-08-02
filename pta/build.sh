#!/bin/sh

#################################################################
 #
 #                       BUILDATOR
 # 
 # Small script to build IMITATOR (tentative !!!)
 #
 # Universite Paris 13, Sorbonne Paris Cite, LIPN (France)
 # 
 # Author:        Etienne Andre
 # 
 # Created:       2013/05/13
 # Last modified: 2013/05/13
 #
################################################################
 
set -e

TARGET=src/IMITATOR
# FLAGS="-libs str,nums,unix,/usr/lib/ocaml/extlib/extLib"
FLAGS="-libs str,unix,/usr/lib/ocaml/extlib/extLib,/usr/lib/ocaml/extlib/dynArray -cflags -I,/usr/lib/ocaml/extlib/,-I,/usr/lib/ocaml/gmp,-I,/usr/lib/ppl -classic-display -Is src -verbose 1"
# , -libs str,unix,extLib,bigarray,gmp,ppl_ocaml
OCAMLBUILD=ocamlbuild

ocb()
{
  $OCAMLBUILD $FLAGS $*
}

rule() {
  case $1 in
    clean)  ocb -clean;;
    native) ocb $TARGET.native;;
    byte)   ocb $TARGET.byte;;
    all)    ocb $TARGET.native $TARGET.byte;;
    depend) echo "Not needed.";;
    *)      echo "Unknown action $1";;
  esac;
}

if [ $# -eq 0 ]; then
  rule all
else
  while [ $# -gt 0 ]; do
    rule $1;
    shift
  done
fi
 
# ocamlbuild - src/IMITATOR.native
