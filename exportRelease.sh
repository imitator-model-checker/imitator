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
