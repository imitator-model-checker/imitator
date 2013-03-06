#!/bin/sh

# Export files for IMITATOR

DESTDIR="/home/etienne/ephemere/release/"

# Create the directory
echo "Creating $DESTDIR"
mkdir $DESTDIR

# Copy root files
for file in GNU.txt INSTALL.txt Makefile RELEASES.txt; do
	echo "Copying $file ..."
	cp $file $DESTDIR$file
done

# Copy all directories
for directory in doc examples src; do
	echo "Copying $directory ..."
	cp -R $directory $DESTDIR$directory
done

# Delete SVN files
cd $DESTDIR
pwd
# find -type d -name '.svn' -exec rm -rfv {} \; 
