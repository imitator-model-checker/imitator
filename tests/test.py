#!/usr/bin/python
# -*- coding: utf-8 -*-
# ************************************************************
#
#                       IMITATOR
#
# Université Sorbonne Paris Nord, LIPN, CNRS, France
# Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
#
# Script description: TESTATOR (script for non-regression tests)
#
# File contributors : Étienne André
# Created           : 2012/05/??
# ************************************************************

# ************************************************************
# MODULES
# ************************************************************
from __future__ import print_function

import time
import datetime
import os
import subprocess
import sys
from collections import namedtuple
import argparse

# Parse arguments
parser = argparse.ArgumentParser()
parser.add_argument("--filter", help="Filter tests to execute", nargs="?", default="")
args = parser.parse_args()

# To output colored text
Colors = namedtuple("Colors", "ERROR, BOLD, GOOD, NORMAL, WARNING, SKIPPED")

bcolors = Colors(
    ERROR="\033[1;37;41m",
    BOLD="\033[1m",
    GOOD="\033[1;32;40m",
    NORMAL="\033[0m",
    WARNING="\033[93;40m",
    SKIPPED="\033[1;37;43m",
)

# ************************************************************
# GENERAL CONFIGURATION
# ************************************************************

# Path to the tests directory
TEST_PATH = os.path.dirname(os.path.abspath(__file__))
# Root path to the main IMITATOR root directory
IMITATOR_PATH = os.path.dirname(TEST_PATH)
# Path to the example directory
EXAMPLE_PATH = os.path.join(TEST_PATH, "testcases/")
# Path to the binary directory
BINARY_PATH = os.path.join(IMITATOR_PATH, "bin/")

# Name for the non-distributed binary to test
BINARY_NAME = "imitator"
# Log file for the non-distributed binary
LOGFILE = os.path.join(TEST_PATH, "tests.log")

# Name for the distributed binary to test
DISTRIBUTED_BINARY_NAME = "patator"
# Log file for the distributed binary
DISTRIBUTED_LOGFILE = os.path.join(TEST_PATH, "testsdistr.log")

# Include long benchmarks in the tests (can be long to execute, but can be useful to check for regressions on long benchmarks)
INCLUDE_LONG_BENCHMARKS = False
# INCLUDE_LONG_BENCHMARKS = True

# Include skipped benchmarks in the tests (can be useful to check for regressions on benchmarks that are currently skipped, but can fail for reasons that are not fixed yet)
INCLUDE_SKIPPED_BENCHMARKS = False
# INCLUDE_SKIPPED_BENCHMARKS = True


# ************************************************************
# BY DEFAULT: ALL TO LOG FILE
# ************************************************************
orig_stdout = sys.stdout
logfile = open(LOGFILE, "w")
sys.stdout = logfile


# ************************************************************
# FUNCTIONS
# ************************************************************
def make_binary(binary):
    return os.path.join(BINARY_PATH, binary)


def make_file(file_name):
    return os.path.join(EXAMPLE_PATH, file_name)


def make_output_file(file_name):
    return os.path.join(file_name)


def fail_with(text):
    print_to_screen(bcolors.ERROR)
    print_to_log(" *** Fatal error: %s" % text)
    print_to_screen("Fatal error!")
    print_to_screen(text + bcolors.NORMAL)
    sys.exit(1)


def print_warning(text):
    print_to_log(" *** Warning: %s" % text)
    print_to_screen(bcolors.WARNING + "Warning: " + text + bcolors.NORMAL)


def print_error(text):
    print_to_log(" *** Error: %s" % text)


# Print text to log file
def print_to_log(content):
    print(content)


def print_to_screen(content, end="\n"):
    # Revert stdout
    sys.stdout = orig_stdout
    # Print
    print(content, end=end)
    # Put back stdout to log file
    sys.stdout = logfile


# Print text both to log file and to screen
# NOTE: can probably do better…
def print_to_screen_and_log(content):
    # Print to log
    print_to_log(content)
    # Also print to screen
    print_to_screen(content)


# ************************************************************
# FORMATS
# ************************************************************
header_benchmark = """

############################################################
 BENCHMARK {benchmark_id}
 purpose: {purpose}
"""

test_fmt = """\
  Test {expectation_id} failed!

*** Expected content for this test:

{expected_content}

*** Content found:

{original_content}


"""

# ************************************************************
# MAIN TESTING FUNCTION
# ************************************************************


def test(binary_name, tests, logfile, logfile_name):
    # *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
    # CHECK FOR THE EXISTENCE OF BINARIES
    # *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
    binary = make_binary(binary_name)
    if not os.path.exists(binary):
        fail_with("Binary `%s` does not exist" % binary)

    print_to_screen(
        "\n{c.BOLD}# TESTING BINARY {name}{c.NORMAL}".format(
            c=bcolors, name=binary_name
        )
    )

    # *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
    # TEST CASES
    # *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
    # Id for benchmarks
    benchmark_id = 1
    # Number of passed benchmarks
    nb_passed_benchmarks = 0
    # Number of skipped benchmarks
    nb_skipped_benchmarks = 0
    nb_toolong_benchmarks = 0

    # Id for test case
    test_case_id = 1
    # Number of passed test cases
    nb_passed_test_cases = 0

    stopwatch_start = time.time()
    for test_case in tests:

        if test_case.get("skip", False) and not INCLUDE_SKIPPED_BENCHMARKS:
            print_to_screen(" Benchmark {} ".format(test_case["purpose"]) + bcolors.SKIPPED + "Skipped!" + bcolors.NORMAL)
            nb_skipped_benchmarks += 1
            continue

        if test_case.get("toolong", False) and not INCLUDE_LONG_BENCHMARKS:
            print_to_screen(" Benchmark {} ".format(test_case["purpose"]) + bcolors.SKIPPED + "Skipped! (too long)" + bcolors.NORMAL)
            nb_toolong_benchmarks += 1
            continue

        # Initially everything is ok
        passed = True

        # Print something
        print_to_log(
            header_benchmark.format(
                benchmark_id=benchmark_id, purpose=test_case["purpose"]
            )
        )
        print_to_screen(
            " Benchmark {}: {}{} … ".format(
                benchmark_id,
                test_case["purpose"],
                " - tags: [{}]".format(test_case["tags"])
                if "tags" in test_case
                else "",
            ),
            "",
        )

        # Add the path to all input files
        # TODO: test for existence of files (just in case)

        # Test existence of each input file
        if test_case.get("bypass_files_existence_check", False):
            print_warning("Bypassing existence check for input files for this test case.")
        else:
            for each_file in test_case["input_files"]:
                if not os.path.exists(make_file(each_file)):
                    fail_with("Input file `{}` does not exist".format(each_file))

        cmd_inputs = [make_file(each_file) for each_file in test_case["input_files"]]

        # ------------------------------------------------------------
        # NOTE: complicated 'if' in case of distributed. Non-distributed: binary = IMITATOR, options = all the rest
        cmd = [binary] + cmd_inputs + (test_case["options"]).split()

        # Distributed: binary = mpiexec, options = all the rest including IMITATOR binary
        if "nb_nodes" in test_case and test_case["nb_nodes"] > 1:
            cmd = ["mpiexec", "-n", str(test_case["nb_nodes"])] + cmd

        # Print the command
        print_to_log(" command : " + " ".join(cmd))

        # Measure the computation time of the command
        start_time = time.time()
        # Launch!
        # NOTE: flushing avoids to mix between results of IMITATOR, and text printed by this script
        logfile.flush()
        subprocess.call(cmd, stdout=logfile, stderr=logfile)
        logfile.flush()
        end_time = time.time()

        # Files to remove
        files_to_remove = set()

        # Check the expectations
        for expectation_id, expectation in enumerate(test_case["expectations"]):
            # Build file
            output_file = make_output_file(expectation["file"])

            test_expectation_id = "{}.{}".format(benchmark_id, expectation_id)

            # Check existence of the output file
            if not os.path.exists(output_file):
                print_to_log(
                    " File {} does not exist! Test {} failed.".format(
                        output_file, test_expectation_id
                    )
                )
                passed = False
            else:
                # Add file to list of files to remove
                files_to_remove.add(output_file)

                # Get extension of file
                _, file_extension = os.path.splitext(output_file)

                if file_extension == ".png":
                    print_to_log(" Test %s passed." % test_expectation_id)
                    nb_passed_test_cases += 1
                else:
                    # Read file
                    with open(output_file, "r") as my_file:
                        # Get the content
                        original_content = my_file.read()
                        # Replace all whitespace characters (space, tab, newline, and so on) with a single space
                        content = " ".join(original_content.split())

                        # Replace all whitespace characters (space, tab, newline, and so on) with a single space
                        expected_content = " ".join(expectation["content"].split())

                        # Look for the expected content
                        position = content.find(expected_content)

                        if position >= 0:
                            print_to_log(" Test %s passed." % test_expectation_id)
                            nb_passed_test_cases += 1
                        else:
                            passed = False
                            print_to_log(
                                test_fmt.format(
                                    expectation_id=test_expectation_id,
                                    expected_content=expectation["content"],
                                    original_content=original_content,
                                )
                            )

        # Update number of test cases
        test_case_id += len(test_case["expectations"])

        # Remove all output files
        for my_file in files_to_remove:
            os.remove(my_file)

        # Print the computation time for this benchmark, rounded to 3 decimal places
        # Highlight the computation time in WARNING if > 0.1 s, in ERROR if > 1 s
        computation_time = " ({} s)".format(str(end_time - start_time)[:5])
        if end_time - start_time > 1:
            computation_time = bcolors.ERROR + computation_time + bcolors.NORMAL
        elif end_time - start_time > 0.1:
            computation_time = bcolors.WARNING + computation_time + bcolors.NORMAL

        # If all test cases passed, increment the number of passed benchmarks
        if passed:
            nb_passed_benchmarks += 1
            print_to_screen(computation_time + " " + bcolors.GOOD + "PASSED!" + bcolors.NORMAL)
        else:
            print_to_screen(computation_time + " " + bcolors.ERROR + "FAILED!" + bcolors.NORMAL)

        # Increment the benchmark id
        benchmark_id += 1

    # *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
    # THE END
    # *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
    stopwatch_end = time.time()

    # Print the summary of the tests
    print_to_log("\n\n############################################################")

    # Display total time, rounded to 3 decimal places   
    print_to_screen_and_log(
        "Total time: {} s".format(str(stopwatch_end - stopwatch_start)[:5])
    )
    # NOTE: ugly…
    total_benchmarks = benchmark_id - 1
    total_test_cases = test_case_id - 1

    if total_benchmarks == nb_passed_benchmarks and total_test_cases == nb_passed_test_cases:
        print_to_screen_and_log(
            "All benchmarks ({}/{}) passed successfully.".format(
                nb_passed_benchmarks, total_benchmarks
            )
        )
        print_to_screen_and_log(
            "All test cases ({}/{}) passed successfully.".format(
                nb_passed_test_cases, total_test_cases
            )
        )
    else:
        print_to_screen(
            bcolors.WARNING + "WARNING! Some tests failed." + bcolors.NORMAL
        )
        print_to_log("WARNING! Some tests failed.")

        if nb_passed_benchmarks == total_benchmarks:
            print_to_screen(
                "{2.GOOD}{0}/{1} benchmarks passed successfully.{2.NORMAL}".format(
                    nb_passed_benchmarks, total_benchmarks, bcolors
                )
            )
        else:
            print_to_screen(
                "{2.WARNING}{0}/{1} benchmarks passed successfully.{2.NORMAL}".format(
                    nb_passed_benchmarks, total_benchmarks, bcolors
                )
            )

        print_to_log(
            "{}/{} benchmarks passed successfully.".format(
                nb_passed_benchmarks, total_benchmarks
            )
        )

        if nb_passed_benchmarks < total_benchmarks:
            print_to_screen(
                "{2.ERROR}{0}/{1} benchmarks failed.{2.NORMAL}".format(
                    total_benchmarks - nb_passed_benchmarks, total_benchmarks, bcolors
                )
            )
        else:
            print_to_screen(
                "{}/{} benchmarks failed.".format(
                    total_benchmarks - nb_passed_benchmarks, total_benchmarks
                )
            )

        print_to_log(
            "{}/{} benchmarks failed.".format(
                total_benchmarks - nb_passed_benchmarks, total_benchmarks
            )
        )

        if nb_passed_test_cases == total_test_cases:
            print_to_screen(
                "{2.GOOD}{0}/{1} test cases passed successfully.{2.NORMAL}".format(
                    nb_passed_test_cases, total_test_cases, bcolors
                )
            )
        else:
            print_to_screen(
                "{2.WARNING}{0}/{1} test cases passed successfully.{2.NORMAL}".format(
                    nb_passed_test_cases, total_test_cases, bcolors
                )
            )

        print_to_log(
            "{}/{} test cases passed successfully.".format(
                nb_passed_test_cases, total_test_cases
            )
        )

        if nb_passed_test_cases < total_test_cases:
            print_to_screen(
                "{2.ERROR}{0}/{1} test cases failed.{2.NORMAL}".format(
                    total_test_cases - nb_passed_test_cases, total_test_cases, bcolors
                )
            )
        else:
            print_to_screen(
                "{}/{} test cases failed.".format(
                    total_test_cases - nb_passed_test_cases, total_test_cases
                )
            )

        print_to_log(
            "{}/{} test cases failed.".format(
                total_test_cases - nb_passed_test_cases, total_test_cases
            )
        )

    if nb_skipped_benchmarks > 0:
        print_to_screen_and_log(
            "\n{1.SKIPPED}{0} benchmarks were skipped (marked as to skip).{1.NORMAL}".format(nb_skipped_benchmarks, bcolors)
        )
    if nb_toolong_benchmarks > 0:
        print_to_screen_and_log(
            "{1.SKIPPED}{0} benchmarks were skipped (marked as too long).{1.NORMAL}".format(nb_toolong_benchmarks, bcolors)
        )

    print_to_screen("(See %s for details.)" % logfile_name)


# ************************************************************
# STARTING SCRIPT
# ************************************************************

# print '*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-'
print_to_screen_and_log("############################################################")
print_to_screen(
    "{0.BOLD} TESTATOR{0.NORMAL}                                              v0.1".format(
        bcolors
    )
)
print_to_log(" TESTATOR                                              v0.1")
print_to_screen_and_log("")
print_to_screen_and_log(" Étienne André")
print_to_screen_and_log(" Université Sorbonne Paris Nord, LIPN, CNRS, France")
print_to_screen_and_log(" Université de Lorraine, CNRS, Inria, LORIA, Nancy, France")
print_to_screen_and_log("############################################################")
now = datetime.datetime.now()
print_to_screen_and_log(now.strftime("%A %d. %B %Y %H:%M:%S %z"))

# ************************************************************
# 1. TESTING IMITATOR
# ************************************************************

# IMPORTING THE TESTS CONTENT
from regression_tests_data import tests

if args.filter:
    # filter structure : --filter "key1=value1, key2=value2"
    # Eventually split on many filters
    all_filters = args.filter.split(",")
    # Split key / value for each filter, obtain list of tuples that represent filters
    tuples = [tuple(f.split("=")) for f in all_filters]
    # Get tests that match with filters
    tests = [
        t
        for t in tests
        if any(k.strip() in t and v.strip() in t[k.strip()] for k, v in tuples)
    ]


test(BINARY_NAME, tests, logfile, LOGFILE)

# ************************************************************
# 2. TESTING PATATOR
# ************************************************************

# # SETTING LOGS
# logfile = open(DISTRIBUTED_LOGFILE, "w")

# # IMPORTING THE TESTS CONTENT
# from regression_tests_data_distr import tests_distr

# test(DISTRIBUTED_BINARY_NAME, tests_distr + tests, logfile, DISTRIBUTED_LOGFILE)

# ************************************************************
# THE END
# ************************************************************

print_to_screen_and_log("\n…The end of TESTATOR!")

sys.exit(0)
