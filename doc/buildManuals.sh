#!/bin/sh

# Build the user manual
# Build developer manual
sh compileManual.sh IMITATOR-user-manual.tex \
&& sh buildDeveloperManual.sh \
&& echo ""\
&& echo "  Both manuals successfully built! :-)"\
