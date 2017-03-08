#!/bin/sh

# Create the class diagram
echo "Handling the class diagrams…"
m4 classDiagramSimplified.m4 > classDiagramSimplified.dot && dot -T png -o classDiagramSimplified.png classDiagramSimplified.dot
m4 classDiagramFull.m4 > classDiagramFull.dot && dot -T png -o classDiagramFull.png classDiagramFull.dot

# Compile developer manual
echo "Compiling the manual…"
pdflatex IMITATOR-not-developer-manual.tex
bibtex IMITATOR-not-developer-manual
pdflatex IMITATOR-not-developer-manual.tex
pdflatex IMITATOR-not-developer-manual.tex
echo "Done"
