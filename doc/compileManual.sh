#!/bin/sh

# Created                : ???
# Made to a better script: 2019/03/20
# Last modification      : 2019/03/20

# Testing the number of parameters
if [ "$#" -ne 1 ]; then
	echo "Usage: $0 file.tex" >&2
	exit 1
fi

# Main input parameter
mainfilewithextension=$1

# Remove extension
mainfile=$(echo "$mainfilewithextension" | cut -f 1 -d '.')


# Test existence
if [ -f "$mainfile.tex" ]
then
	echo "$mainfile.tex found."
else
	echo "$mainfile.tex not found :("
	exit 1
fi

# Internal variable
# NOTE: booh, not beautiful!!
tempfileprefix=$mainfile-temp12345

# TODO: test that biber is installed!!!

echo "\n############################################################"
echo "  Hello :-) Compiling $mainfile…"
echo "############################################################"

echo "\n############################################################"
echo "  REMOVING TEMP FILES…"
echo "############################################################"
rm $tempfileprefix.*
echo "\n############################################################"
echo "  COPY LaTeX source…"
echo "############################################################"
cp $mainfile.tex $tempfileprefix.tex \
&& echo "\n############################################################"\
&& echo "  PDFLATEX 1"\
&& echo "############################################################"\
&& pdflatex $tempfileprefix.tex\
&& echo "\n############################################################"\
&& echo "  BIBER"\
&& echo "############################################################"\
&& biber $tempfileprefix\
&& echo "\n############################################################"\
&& echo "  PDFLATEX 2"\
&& echo "############################################################"\
&& pdflatex $tempfileprefix.tex\
&& echo "\n############################################################"\
&& echo "  PDFLATEX 3"\
&& echo "############################################################"\
&& pdflatex $tempfileprefix.tex\
&& echo "\n############################################################"\
&& echo "  COPYING PDF…"\
&& cp $tempfileprefix.pdf $mainfile.pdf \
&& echo "  REMOVING TEMP FILES…"\
&& rm $tempfileprefix.* \
&& echo ""\
&& echo "  DONE :-)"\
&& echo "############################################################"\
&& echo ""

