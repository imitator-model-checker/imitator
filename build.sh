#!/bin/sh

#################################################################
 #
 #                       BUILDATOR
 # 
 # Small script to build IMITATOR
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
FLAGS="-cflags -I,/usr/lib/ocaml/gmp,-I,/usr/lib/ocaml/extlib -Is src -libs gmp,str,extLib,bigarray"
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
