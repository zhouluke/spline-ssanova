#!/bin/bash

FILES=/media/luke/LUKEY-RED/AAA-EXPORTED-RED/*/*.TextGrid

NEW_DIR=/media/luke/LUKEY-RED/MA-SUBSET/

mkdir -p "$NEW_DIR";

for file in $FILES
do
	
	[ -e "$file" ] || continue
  	
  	textGridFile="$file"
	myWav=${file%.*TextGrid}".wav"
	myTxt=${file%.*TextGrid}".txt"

	spkCondDir="$(basename $(dirname $file))"

	newName="$spkCondDir-$(head -1 $myTxt | tr -d '\n')"
	#newName="$($newName)"

	echo "Now copying $newName"

	# Copies both the TextGrid and its corresponding wav to a new directory
  	cp "$textGridFile" "$NEW_DIR/$newName.TextGrid"
  	cp "$myWav" "$NEW_DIR/$newName.wav"
  	 
done