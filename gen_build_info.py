#!/usr/bin/python
# -*- coding: utf-8 -*-

# ************************************************************
#
#                       IMITATOR
#
#               Create module BuildInfo
#
# Etienne Andre
#
# Laboratoire d'Informatique de Paris Nord
# Universite Paris 13, France
#
# Created      : 2013/09/26
# Last modified: 2018/02/28
# ************************************************************

from __future__ import print_function

import subprocess
from time import gmtime, strftime

# ************************************************************
# CONSTANTS
# ************************************************************
build_number_file_name = "build_number.txt"
ml_file_name = "src/BuildInfo.ml"
mli_file_name = "src/BuildInfo.mli"


print("Python is now handling build information…")


# ************************************************************
# GET CURRENT BUILD TIME
# ************************************************************
current_build_date = strftime("%Y-%m-%d %H:%M:%S", gmtime()) + " UTC"
# Just for generation date
date_str = strftime("%Y-%m-%d", gmtime())
year_str = strftime("%Y", gmtime())


# ************************************************************
# GET CURRENT BUILD NUMBER
# ************************************************************
# Open file in read mode
with open(build_number_file_name) as file_handler:
    # Read content
    content = file_handler.read()

    # Convert to int
    current_build = int(content)


# ************************************************************
# TRY TO GET GIT INFORMATION
# ************************************************************
ocaml_fmt = 'Some "{}"'
git_fmt = 'Retrieved git {}: {}'


def get_ocaml_info(info):
    """Method that gets specific information from git and returns a typed value for Ocaml"""
    if info == 'hash':  # NOTE: command is 'git rev-parse HEAD'
        git_command = ["git", "rev-parse", "HEAD"]
    elif info == 'branch':
        git_command = ["git", "rev-parse", "--abbrev-ref", "HEAD"]
    else:
        raise NotImplemented

    try:
        git_info = (subprocess.check_output(git_command)).rstrip().decode('utf-8')
    except subprocess.CalledProcessError:  # Case: exception with problem (typically return code <> 1)
        # nothing
        git_info = ""

    print(git_fmt.format(info, git_info))

    # Handle what to print in Ocaml
    git_ocaml = ocaml_fmt.format(git_info)
    if git_info == "":
        git_ocaml = "None"

    return git_ocaml


# 1) Retrieve the git hash number
git_hash_ocaml = get_ocaml_info('hash')


# 2) Retrieve the branch
git_branch_ocaml = get_ocaml_info('branch')


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
 * Universite Paris 13, LIPN (France)
 * 
 * Author:        python script
 * 
 * Automatically generated: {date} 
 *
 ****************************************************************)
 
let build_number = "{current_build}"
let build_time = "{current_build_date}"
let build_year = "{year}"
let git_branch = {git_branch}
let git_hash = {git_hash}

"""

write_to_file(ml_file_name, ml_fmt.format(date=date_str,
                                          current_build=current_build,
                                          current_build_date=current_build_date,
                                          year=year_str,
                                          git_branch=git_branch_ocaml,
                                          git_hash=git_hash_ocaml))


# .mli
mli_fmt = """
(*****************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Universite Paris 13, LIPN (France)
 * 
 * Author:        python script
 * 
 * Automatically generated: {date}
 *
 ****************************************************************)
 
val build_number : string
val build_time   : string
val build_year   : string
val git_branch   : string option
val git_hash     : string option
"""

write_to_file(mli_file_name, mli_fmt.format(date=date_str))


print("Files '{}' and '{}' successfully generated.".format(ml_file_name, mli_file_name))

exit(0)
