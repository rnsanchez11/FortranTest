
set terminal png size 1200,941 crop
set output 'testimage.png'

set xlabel 'time [sec]'
set ylabel 'radius [m]'
set title 'my firts plot'

plot sin(x)/x