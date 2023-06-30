# plot.plt

#set title "{/:Bold Mean Velocity Profile}" font "Times-New-Roman,18"
set nokey
set grid
#set key right top
#set xrange [0:1.05]
#set yrange [-1.01:1.01]
set xlabel "<U>/Uc" font "Times-New-Roman,14"
set ylabel "y/d" font "Times-New-Roman,14"
Umean = "plotmeanvelx.txt"
Ulaminar = "datalaminar.txt"
plot Umean with lines lt rgb "red", Ulaminar with lines lt rgb "blue"
