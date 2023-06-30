# plot.plt

#set title "{/:Bold Mean Velocity Profile}" font "Times-New-Roman,18"
set nokey
#set grid
set key right top
set xrange [0:1.1]
#set yrange [-1.01:1.01]
#set xlabel "<U>/Uc" font "Times-New-Roman,14"
#set ylabel "y/d" font "Times-New-Roman,14"
U0 = "Uxmean_0000.txt"
U10 = "Uxmean_0010.txt"
U20 = "Uxmean_0020.txt"
U30 = "Uxmean_0030.txt"
U50 = "Uxmean_0050.txt"
U70 = "Uxmean_0070.txt"
U100 = "Uxmean_0100.txt"
U200 = "Uxmean_0200.txt"
U300 = "Uxmean_0300.txt"
U400 = "Uxmean_0400.txt"

plot U0 with lines lt rgb "red", U10 with lines, U20 with lines, U30 with lines, U50 with lines, U70 with lines, U100 with lines, U200 with lines, U300 with lines, U400 with lines
