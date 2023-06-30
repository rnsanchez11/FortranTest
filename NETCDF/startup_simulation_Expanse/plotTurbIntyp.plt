# plot.plt

#set title "{/:Bold Turbulence intensities}" font "Times-New-Roman,18"
set nokey
#set grid
set key right top
#set xrange [0:1.05]
#set yrange [-1.01:1.01]
#set ylabel "u rms, v rms, w rms" font "Times-New-Roman,14"
#set xlabel "y+" font "Times-New-Roman,14"
#set view equal xy
fluctuationsx = "dataTurbIntypx.txt"
fluctuationsy = "dataTurbIntypy.txt"
fluctuationsz = "dataTurbIntypz.txt"
plot fluctuationsx with lines lt rgb "red", fluctuationsy with lines lt rgb "blue", fluctuationsz with lines lt rgb "green"