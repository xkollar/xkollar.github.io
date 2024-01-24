# vim: ft=gnuplot

set key inside left box
set xtics (0, 12570, 12570+37700, 100000, 125140, 165000, 200000) nomirror
set yrange [0:70]
set format x "£%.0f"
set format y2 "£%.0f"
set format y "%.0f%%"
set grid xtics ytics y2tics

set lmargin 6
set rmargin 9
set tmargin 2
set bmargin 3

print "hello"

# set term svg size 1200,600 dynamic enhanced mousing
# set output "output.svg"

set term pngcairo size 1200,800
set output "tax-effective-and-perceived-2023-2024.png"

set multiplot title "Tax Effective and Perceived 2023-2024"

set key inside left box offset +3
set yrange [0:70]
set ytics (12, 22, 32, 39, 42, 47, 52, 62) nomirror
plot \
    'tax-effective-and-perceived-2023-2024.dat' using 1:(100*($2+$3)/$1) axes x1y1 with line linestyle 4 linewidth 5 title 'Effective tax', \
    "<awk '{print $1, $2+$3-acc; acc=$2+$3}' tax-effective-and-perceived-2023-2024.dat" using 1:(100*$2/100) axes x1y1 with line linestyle 5 linewidth 5 title 'Perceived tax on last £100'

unset grid
set key inside right box opaque offset -3
unset ytics
unset yrange
set y2tics
plot \
    'tax-effective-and-perceived-2023-2024.dat' every 50::0 using 1:2 axes x1y2 with linespoints linewidth 2 title "National Insurance", \
    'tax-effective-and-perceived-2023-2024.dat' every 50::0 using 1:3 axes x1y2 with linespoints linewidth 2 title "Income Tax", \
    'tax-effective-and-perceived-2023-2024.dat' every 50::0 using 1:($2+$3) axes x1y2 with linespoints linewidth 2 title "Income + NI"

unset multiplot
