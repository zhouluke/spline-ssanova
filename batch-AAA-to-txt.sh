#!/bin/bash

echo "Processing BM"
echo ".....Doing s-sh"
python AAA-to-csv.py "BM" "../Splines/s-sh/BM-s-sh-C.txt" > "all-polar-data.txt" 
echo ".....Doing k-t"
python AAA-to-csv.py "BM" "../Splines/k-t/BM-k-t.txt" >> "all-polar-data.txt" 

for i in `seq 2 9`; do
	echo "Processing TP$i"
	echo ".....Doing s-sh"
	python AAA-to-csv.py "TP$i" "../Splines/s-sh/TP$i-s-sh.txt" >> "all-polar-data.txt" #"TP$i.txt"
	echo ".....Doing k-t"
	python AAA-to-csv.py "TP$i" "../Splines/k-t/TP$i-k-t.txt" >> "all-polar-data.txt" #"TP$i.txt"
done