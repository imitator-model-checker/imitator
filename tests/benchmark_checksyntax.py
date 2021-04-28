#!/usr/bin/python
# -*- coding: utf-8 -*-
# ************************************************************
#
#                       IMITATOR
#
# Université Paris 13, LIPN, CNRS, France
# Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
#
# Script description: Benchmark Check syntax (script for checking syntax of all benchmark model)
#
# File contributors : Benjamin L.
# Created           : 2021/04/26
# Last modified     : 2021/04/26
# ************************************************************

from pathlib import Path
import subprocess
import os
import os.path
import uuid
import argparse
import sys
import tempfile
import shutil




def create_sandbox():
    # Prepare sandbox
    temp_dir = tempfile.gettempdir()
    sandbox_dir = os.path.join(temp_dir, 'benchmark_checksyntax_' + str(uuid.uuid1()))
    os.mkdir(sandbox_dir)

    print("Sandbox created : " + sandbox_dir)
    return sandbox_dir

def check_syntax(sandbox_dir):
    models = Path('../benchmarks').rglob("*.[iI][mM][iI]")
    count = 0
    error_models = []
    for model in models:
        print(model.name + " - check syntax")
        print("----------------------------------")
        result = subprocess.run(['imitator', '-mode', 'checksyntax', model.absolute()], cwd=sandbox_dir, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
        if 'ERROR' in result.stderr:
            print(model.name + ': error')
            error_models.append(model.absolute())
        else:
            print(model.name + ': ok')

        print("----------------------------------")
        # input('press key next')

    print(str(len(error_models)) + "/" + str(len(list(models))) + " syntax error(s)")
    print("list of models with errors:")
    for model in error_models:
        print(model)

# Cleanup generated files
def cleanup_sandbox(sandbox_dir):
    # Cleanup files
    shutil.rmtree(sandbox_dir)

def __main__():
    sandbox_dir = create_sandbox()
    check_syntax(sandbox_dir)
    # cleanup_sandbox(sandbox_dir)

__main__()