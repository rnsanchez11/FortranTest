# plot.plt

set title "{/:Bold Mean Velocity Profile in bold}"
set nokey
set grid
set key right top
#set xrange [0:1.05]
#set yrange [-1.01:1.01]
set xlabel "<U>/U"
set ylabel "y/d"
#set view equal xy
fluctuations = "data.txt"
plot fluctuations with lines lt rgb "red"
