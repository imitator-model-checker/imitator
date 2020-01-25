The artefact provides the source files as well as a compiled version of imitator.

*********************************
0. Move to the imitator directory
*********************************

All files are in the directory named imitator.

cd imitator

The specific implementation of NDFS source code is in the file: src/AlgoNDFS.ml

********************************
1. The executable file
********************************
The executable of the tool is:

bin/imitator

The execution without options/automata file provides a summary of usage of the tool.
The full detailed manual is in docs/IMITATOR-user-manual.pdf

********************************
2. Running the experiments
********************************
Two scripts are provided that were used for the experiments in the paper:

- generate_table.sh generates table 2 contents in csv format which can be viewed in a spreadsheet tool (the delimitor used is ; which is standard).
The script requires an output file specified with the -o option, e.g.:

./generate_table.sh -o table.csv

The script also provides means to specify a timeout (other than the 90 seconds by default), use a subset of the models or give a list of models. The help option explains this:

./generate_table.sh -h


- brp_experiments.sh executes all experiments on the Bounded Retransmission Protocol in the same order as they appear in the paper. The output is not processed by the script, hence the user sees the partial results obtained on-the-fly by imitator. To enhance readability:
	* each series of experiments in a section has a title with a green background
	* the commands executed are displayed with a yellow background before execution
	* The final results of imitator are easily located as the constraint is displayed with a black background 

