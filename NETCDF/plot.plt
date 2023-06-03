# plot.plt

set title "Fortran Example"
set nokey
set grid
set xlabel "y/d"
set ylabel "<U>/U"
m="data.txt"
plot m using 1:2 with linespoints