cat out/*.csv > allmain.csv
cat ISAOut1/*.csv > allISA1.csv
cat ISAOut2/*.csv > allISA2.csv
cat ISAOut3/*.csv > allISA3.csv
cat ISAOut4/*.csv > allISA4.csv

cat CreateRData.R | R --vanilla -q
