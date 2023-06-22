# plot.plt

set title "{/:Bold Turbulence intensities}" font "Times-New-Roman,18"
set nokey
set grid
set key right top
#set xrange [0:1.05]
#set yrange [-1.01:1.01]
set ylabel "u rms, v rms, w rms" font "Times-New-Roman,14"
set xlabel "y/δ" font "Times-New-Roman,14"
#set view equal xy
fluctuationsx = "dataTurbIntx.txt"
fluctuationsy = "dataTurbInty.txt"
fluctuationsz = "dataTurbIntz.txt"
plot fluctuationsx with lines lt rgb "red", fluctuationsy with lines lt rgb "blue", fluctuationsz with lines lt rgb "green"