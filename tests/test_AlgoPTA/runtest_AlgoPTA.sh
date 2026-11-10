#!/bin/bash

##################################################
#               ARGUMENT CHECKING                #
##################################################
if [ $# -ne 2 ]; then
	echo "Use: runtest_AlgoPTA.sh -f filename.imi"
	exit 1
fi

##################################################
#                ARGUMENT READING                #
##################################################
while getopts f: option
do
	case "${option}"
	in
		f) FILENAME=${OPTARG};;
	esac
done

##################################################
#                STATIC VARIABLES                #
##################################################
OUT_DIR="./tests/test_AlgoPTA/"
IMI_BIN="./bin/imitator"
PTA_OPT="-pta-sync-prod"

##################################################
#           ARRAY WITH OPTIONS TO TEST           #
##################################################
declare -a OPTIONS=(
	"-acyclic"
	"-cart-tiles-limit"
	"-cart-time-limit"
	"-check-ippta"
	"-check-point"
	"-comparison none"
	"-comparison equality"
	"-comparison inclusion"
	"-comparison including"
	"-comparison doubleinclusion"
	"-cycle-algo BFS"
	"-cycle-algo NDFS"
	"-depth-init"
	"-depth-limit"
	"-depth-step"
	"-distributed no"
	"-distributed static"
	"-distributed sequential"
	"-distributed randomXX"
	"-distributed shuffle"
	"-distributed dynamic"
	"-distributed-kill-IM"
	"-draw-cart"
	"-draw-cart-x-min"
	"-draw-cart-x-max"
	"-draw-cart-y-min"
	"-draw-cart-y-max"
	"-draw-statespace undetailed"
	"-draw-statespace normal"
	"-draw-statespace full"
	"-dynamic-elimination"
	"-no-global-time-clock-in-comparison"
	"-expl-order layerBFS"
	"-expl-order queueBFS"
	"-expl-order queueBFSRS"
	"-expl-order queueBFSPRIOR"
	"-extrapolation M"
	"-extrapolation Mglobal"
	"-extrapolation LU"
	"-extrapolation LUglobal"
	"-graphics-source"
	"-graphics-detail full"
	"-graphics-detail minimal"
	"-ih"
	"-imi2DOT"
	"-imi2HyTech"
	"-imi2IMI"
	"-imi2imiprop"
	"-imi2Jani"
	"-imi2JPG"
	"-imi2PDF"
	"-imi2PNG"
	"-imi2TikZ"
	"-imi2Uppaal"
	"-layer"
	"-no-layer"
	"-merge none"
	"-merge yes"
	"-merge reconstruct"
	"-merge onthefly"
	"-merge 2.12"
	"-merge-candidates queue"
	"-merge-candidates visited"
	"-merge-candidates ordered"
	"-merge-update merge"
	"-merge-update candidates"
	"-merge-EFsynthminpq-heuristic always"
	"-merge-EFsynthminpq-heuristic targetseen"
	"-merge-EFsynthminpq-heuristic pq10"
	"-merge-EFsynthminpq-heuristic pq100"
	"-merge-EFsynthminpq-heuristic iter10"
	"-merge-EFsynthminpq-heuristic iter100"
	"-merge-restart on"
	"-merge-restart off"
	"-mode checksyntax"
	"-mode statespace"
	"-new-queue-EF"
	"-no-acceptfirst"
	"-no-cache-in-AF"
	"-no-coverage-pruning"
	"-no-cumulative-pruning"
	"-no-green"
	"-no-lookahead"
	"-no-random"
	"-no-var-autoremove"
	"-nz-method check"
	"-nz-method transform"
	"-output-float"
	"-output-prefix"
	"-no-output-result"
	"-pending-order accepting"
	"-pending-order none"
	"-pending-order param"
	"-pending-order zone"
	"-precomputepi0"
	"-PTG-generate-controller no-draw"
	"-PTG-generate-controller draw"
	"-PTG-propagate"
	"-PTG-no-forced-uncontrollables"
	"-PTG-no-strategy-printing"
	"-PTG-no-strategy-generation"
	"-PTG-waiting-list-strategy queue"
	"-PTG-waiting-list-strategy frontier"
	"-PTG-frontier-params"
	"-PTG-abstraction location"
	"-PTG-abstraction ch"
	"-PTG-abstraction none"
	"-hull-method convex"
	"-hull-method box"
	"-hull-method octagonal"
	"-hull-method box-hybrid"
	"-hull-method octagonal-hybrid"
	"-hull-simplify none"
	"-hull-simplify constraints"
	"-hull-simplify generators"
	"-hull-simplify-period"
	"-hull-abstraction-threshold"
	"-recompute-green"
	"-states-description"
	"-states-limit"
	"-statistics"
	"-subsumption"
	"-no-subsumption"
	"-sync-auto-detect"
	"-tiles-files"
	"-time-elapsing-after"
	"-time-limit"
	"-timed"
)
  
##################################################
#                 STARTING TESTS                 #
##################################################
echo "#######################################################"
echo "Running tests for PTA Synchronized Cartesian Product..."
echo "#######################################################"
echo ""

#	ITERATE OVER ALL OPTIONS
for option in "${OPTIONS[@]}"
do
#	CREATE COMMAND TO RUN
	CMD="$IMI_BIN $FILENAME $PTA_OPT $option"

#	GET ONLY THE FILE NAME (WITHOUT PATH AND EXTENSION)
	NAME=$(basename -- "$FILENAME")
	NAME="${NAME%.*}"

#	ERASE THE FIRST "-" AND REPLACE SPACES BY "_"
	OPT=${option#*-}
	OPT=${OPT/ /_}

#	CREATE THE OUTPUT FILENAME
	OUTPUT="${OUT_DIR}${NAME}_${OPT}.txt"

#	RUN AND WRITE
	echo "Running test: [$CMD]"
	eval $CMD &> $OUTPUT
	echo "Output written to file [$OUTPUT]"
done
