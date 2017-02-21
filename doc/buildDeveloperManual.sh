#!/bin/sh

# Create the class diagram
echo "Handling the class diagram…"
dot -T png -o classDiagram.png classDiagram.dot

# Compile developer manual
echo "Compiling the manual…"
pdflatex IMITATOR-not-developer-manual.tex
bibtex IMITATOR-not-developer-manual
pdflatex IMITATOR-not-developer-manual.tex
pdflatex IMITATOR-not-developer-manual.tex
echo "Done"
