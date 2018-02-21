#!/usr/bin/python
# -*- coding: utf-8 -*-

# ************************************************************
#
#                       IMITATOR
#
# Create the _oasis file for IMITATOR
#
# Etienne Andre
#
# Laboratoire d'Informatique de Paris Nord
# Universite Paris 13, Sorbonne Paris Cite, France
#
# Created      : 2014/08/18
# Last modified: 2015/07/22
# ************************************************************


# This script copies oasis-config-distr into _oasis


# ************************************************************
# IMPORTS
# ************************************************************
from gen_oasis import write_to_oasis

# ************************************************************
# CONSTANTS
# ************************************************************
# Files
input_file_path = 'oasis-config-distr'
output_file_path = '_oasis'


# WARNING: the rest of this file is identical to gen_oasis.py

# ************************************************************
# GO
# ************************************************************
if __name__ == '__main__':
    write_to_oasis(input_file_path, output_file_path)
