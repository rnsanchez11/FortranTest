# plot.plt

set title "{/:Bold Mean Velocity Profile in bold}"
set nokey
set grid
set key right top
#set xrange [0:1.05]
#set yrange [-1.01:1.01]
set xlabel "<U>/U" 
set ylabel "y/d" 
Umean = "data.txt"
Ulaminar = "datalaminar.txt"
plot Umean with lines lt rgb "red", Ulaminar with lines lt rgb "blue"
