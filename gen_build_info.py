#!/usr/bin/python
# -*- coding: utf-8 -*-

# ************************************************************
#
#                       IMITATOR
#
#               Create module BuildInfo
#
# Étienne André
#
# Université Sorbonne Paris Nord, LIPN, CNRS, France
# Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
#
# Created      : 2013/09/26
# Last modified: 2020/12/15
# ************************************************************

from __future__ import print_function

import os
import subprocess
import sys
from time import gmtime, strftime

# ************************************************************
# CONSTANTS
# ************************************************************
if len(sys.argv) == 3:
    ml_file_name = sys.argv[1]
    mli_file_name = sys.argv[2]
else:
    raise SystemExit(
        "Usage: {} [ML_OUTPUT MLI_OUTPUT]".format(os.path.basename(sys.argv[0]))
    )

print("Python is now handling build information…")

# ************************************************************
# GET CURRENT BUILD TIME
# ************************************************************
current_build_date = strftime("%Y-%m-%d %H:%M:%S", gmtime()) + " UTC"
# Just for generation date
date_str = strftime("%Y-%m-%d", gmtime())
year_str = strftime("%Y", gmtime())

# ************************************************************
# TRY TO GET GIT INFORMATION
# ************************************************************
ocaml_fmt = 'Some "{}"'
git_fmt = "Retrieved git {}: {}"


def ocaml_string(value):
    """Escapes a Python string for a generated OCaml string literal."""
    return value.replace("\\", "\\\\").replace('"', '\\"')


def env_info(info):
    """Returns git information provided by the build environment, if any."""
    if info == "hash":
        return os.environ.get("IMITATOR_GIT_HASH") or os.environ.get("GITHUB_SHA")
    elif info == "branch":
        return os.environ.get("IMITATOR_GIT_BRANCH") or os.environ.get("GITHUB_REF_NAME")
    else:
        raise NotImplementedError


def get_ocaml_info(info):
    """Method that gets specific information from git and returns a typed value for Ocaml"""
    if info == "hash":  # NOTE: command is 'git rev-parse HEAD'
        git_command = ["git", "rev-parse", "HEAD"]
    elif info == "branch":
        git_command = ["git", "rev-parse", "--abbrev-ref", "HEAD"]
    else:
        raise NotImplementedError

    try:
        git_info = (subprocess.check_output(git_command)).rstrip().decode("utf-8")
    except Exception:  # Case: exception with problem (typically return code <> 1)
        git_info = env_info(info)
        if git_info:
            print("Error with git: using {} from build environment".format(info))
        else:
            print("Error with git: give up git information")
            git_info = "?????"

    print(git_fmt.format(info, git_info))

    # Handle what to print in Ocaml
    git_ocaml = ocaml_fmt.format(ocaml_string(git_info))
    if git_info == "":
        git_ocaml = "None"

    return git_ocaml


# 1) Retrieve the git hash number
git_hash_ocaml = get_ocaml_info("hash")

# 2) Retrieve the branch
git_branch_ocaml = get_ocaml_info("branch")


# ************************************************************
# CREATES OCAML FILES
# ************************************************************
def write_to_file(file_name, content):
    """Method to write into a specific file."""
    with open(file_name, "w") as file_handler:
        # Write content
        file_handler.write(content)


# .ml
ml_fmt = """
(*****************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN (France)
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Author:        python script
 *
 * Automatically generated: {date}
 *
 ****************************************************************)

let build_time = "{current_build_date}"
let build_year = "{year}"
let git_branch = {git_branch}
let git_hash = {git_hash}

"""

write_to_file(
    ml_file_name,
    ml_fmt.format(
        date=date_str,
        current_build_date=current_build_date,
        year=year_str,
        git_branch=git_branch_ocaml,
        git_hash=git_hash_ocaml,
    ),
)

# .mli
mli_fmt = """
(*****************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN (France)
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Author:        python script
 *
 * Automatically generated: {date}
 *
 ****************************************************************)

val build_time   : string
val build_year   : string
val git_branch   : string option
val git_hash     : string option
"""

write_to_file(mli_file_name, mli_fmt.format(date=date_str))

print("Files '{}' and '{}' successfully generated.".format(ml_file_name, mli_file_name))

exit(0)
