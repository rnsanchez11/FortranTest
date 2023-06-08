# plot.plt

set title "{/:Bold Mean Velocity Profile in bold}" font "Bookman Old Style, 16, bold"
set nokey
set grid
set key right top font "Bookman Old Style, 12"
set xrange [0:1.05]
set yrange [-1.01:1.01]
set xlabel "<U>/U" font "Bookman Old Style, 14"
set ylabel "y/d" font "Bookman Old Style, 14"
Umean = "data.txt"
Ulinear = "datalinear.txt"
plot Umean with lines lt rgb "red" lw 2, Ulinear with lines lt rgb "blue"
