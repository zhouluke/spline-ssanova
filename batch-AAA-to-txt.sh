#!/bin/bash

for i in `seq 6 8`; do
	echo "Processing TP$i"
	python AAA-to-csv.py "TP$i" "../Splines/s-sh/TP$i-s-sh-cart.txt" > "TP$i.txt"
	python AAA-to-csv.py "TP$i" "../Splines/k-t/TP$i-k-t-cart.txt" >> "TP$i.txt"
done