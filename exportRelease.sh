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

# Create bin and lib
for directory in bin lib; do
	echo "Creating $directory ..."
	mkdir $DESTDIR$directory
done

# Delete SVN files ; the "cd" command is very important! otherwise SVN files are deleted in the source...
echo "cd $DESTDIR"
cd $DESTDIR

# pwd

find -type d -name '.svn' -exec rm -rfv {} \; 

for extension in jpg pdf ; do
# 	find . -iname "*.$extension" -ok rm '{}' ';'
done
