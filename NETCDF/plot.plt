# plot.plt

set title "Mean Velocity Profile"
set nokey
set grid
set xlabel "<U>/U"
set ylabel "y/d"
m="data.txt"
plot m using 1:2 with linespoints