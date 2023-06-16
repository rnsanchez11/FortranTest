# plot.plt

set title "{/:Bold Mean Velocity Profile in bold}" font "16"
set nokey
set grid
set key right top font "12"
#set xrange [0:1.05]
#set yrange [-1.01:1.01]
set xlabel "<U>/U" font "14"
set ylabel "y/d" font "14"
Umean = "Onepoint.txt"
plot Umean with lines lt rgb "red", #Ulaminar with lines lt rgb "blue"
