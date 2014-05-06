#!/bin/sh

#################################################################
 #
 #                       RELEASATOR
 # 
 # Small script to export releases from IMITATOR
 #
 # Universite Paris 13, Sorbonne Paris Cite, LIPN (France)
 # 
 # Author:        Etienne Andre
 # 
 # Created:       2013/03/06
 # Last modified: 2014/05/06
 #
################################################################
 

# Name of the destination directory
# NOTE: everything will be erased (if anything exists before the script)
DESTDIR="/home/etienne/ephemere/release/"


echo "\n STARTING RELEASATOR"

# Create the directory
echo "Creating $DESTDIR"
mkdir $DESTDIR

# Remove everything in the directory (just in case)
echo "First cleaning $DESTDIR"
rm -rf $DESTDIR/*

# Copy root files
for file in build_number.txt GNU.txt INSTALL.txt _oasis makeoasis.sh gen_build_info.py incrementer.py RELEASES.txt; do
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

# WARNING: The "cd" command is very important! otherwise files are deleted in the source...
echo "cd $DESTDIR"
cd $DESTDIR

# Delete useless files
for extension in .jpg "~" .cmi .cmo .states .ps; do
	# Remplacer "-exec" par "-ok" demande confirmation a chaque fichier
	echo "Deleting files with extension \"$extension\" ..."
	find . -iname "*$extension" -exec rm '{}' ';'
done

# Delete SVN files
echo "Deleting .svn directories..."
find -type d -name '.svn' -exec rm -rfv {} \; 

echo "The end!"
