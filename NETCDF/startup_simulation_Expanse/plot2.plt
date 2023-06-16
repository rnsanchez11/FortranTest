# plot.plt

set title "{/:Bold Mean Velocity Profile}" font "Times-New-Roman,18"
set nokey
set grid
set key right top
#set xrange [0:1.05]
#set yrange [-1.01:1.01]
set xlabel "<U>/U" font "Times-New-Roman,14"
set ylabel "y/d" font "Times-New-Roman,14"
#set view equal xy
fluctuations = "data.txt"
plot fluctuations with lines lt rgb "red"
