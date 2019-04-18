cat out/*.csv > allmain.csv
cat ISAOut1/*.csv > allISA1.csv

cat CreateRData.R | R --vanilla -q
