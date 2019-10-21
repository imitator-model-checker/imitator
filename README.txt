The artifact provides the source files as well as a compiled version of imitator.

********************************
1. Compiling the tool (optional)
********************************
run the script:

sh ./build.sh

Note that the artifact provides all dependencies for compiling. Additional information for building the tool can be found on the official web page (https://www.imitator.fr/download.html).

The specific implementation of NDFS source code is in the file: src/AlgoNDFS.ml

********************************
2. The executable file
********************************
The executable of the tool is:

bin/imitator

The execution without options/automata file provides a summary of usage of the tool.
The full detailed manual is IMITATOR-user-manual.pdf

********************************
3. Running the experiments
********************************
Four scripts are provided that were used for the experiments in the paper:

- brp_experiments.sh executes all experiments on the Bounded Retransmission Protocol in the same order as they appear in the paper

- generate_table[2-4].sh generates tables 2 and 3 from the paper plus an additional one containing experiments on the different orders proposed for the pending list.
Since some examples timeout, it may be wise to comment out the lengthy examples.
These scripts use two arguments: the timeout in seconds (set to 900 for the paper) and the filename for the produced LaTeX table.
The tables obtained that way were modified to report only 2 decimals in timings and with colours in some cells for a better presentation in the paper.

example:
./generate_table2.sh 900 table2.tex
