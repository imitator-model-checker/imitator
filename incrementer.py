#!/usr/bin/python
#************************************************************
#
#                       IMITATOR
#
#               Increment the build number
#
# Etienne ANDRE
# Laboratoire d'Informatique de Paris Nord
# Universite Paris 13, Sorbonne Paris Cite, France
# Created      : 2013/05/22
# Last modified: 2013/05/22
#************************************************************


file_name = "build_number.txt"

# Open file in read mode
file_handler = open(file_name)

# Read content
content = file_handler.read()

# Close file
file_handler.close()

# Convert to int
current_build = int(content)

# Open file in write mode
file_handler = open(file_name, "w")

# Add + 1
file_handler.write(str(current_build + 1))

# Close file
file_handler.close()

exit(0)
