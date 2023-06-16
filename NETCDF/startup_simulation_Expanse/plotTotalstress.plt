# plot.plt

set title "{/:Bold Profiles of the viscous and Reynolds shear stress}" font "Times-New-Roman,18"
set nokey
set grid
set key right top
#set xrange [0:1.05]
#set yrange [-1.01:1.01]
set xlabel "<U>/U" font "Times-New-Roman,14"
set ylabel "y/d" font "Times-New-Roman,14"
totalShearStress = "totalshear.txt"
viscousStress = "viscoustress.txt"
#ReynoldsStress = "Restress.txt"
plot totalShearStress with lines lt rgb "red", viscousStress with lines lt rgb "blue" #, ReynoldsStress with lines lt rgb "green"
#plot viscousStress with lines lt rgb "blue"