# vim: ft=gnuplot

set title "Tax Effective and Perceived 2023-2024"
set key inside left box
set xtics (0, 12570, 12570+37700, 100000, 125140, 165000, 200000)
set ytics nomirror
set y2range [0:70]
set y2tics (0, 10, 20, 30, 40, 45, 50, 60, 65, 70)
set format x "£%.0f"
set format y "£%.0f"
set format y2 "%.0f%%"
set grid xtics ytics y2tics

# set term svg size 1200,600 dynamic enhanced mousing
# set output "output.svg"

# set term png size 1200,600
set term pngcairo size 1200,600
set output "tax-effective-and-perceived-2023-2024.png"

plot \
    'tax-effective-and-perceived-2023-2024.dat' every 50::0 using 1:2 with linespoints linewidth 2 title "National Insurance", \
    'tax-effective-and-perceived-2023-2024.dat' every 50::0 using 1:3 with linespoints linewidth 2 title "Income Tax", \
    'tax-effective-and-perceived-2023-2024.dat' every 50::0 using 1:($2+$3) with linespoints linewidth 2 title "Income + NI", \
    'tax-effective-and-perceived-2023-2024.dat' using 1:(100*($2+$3)/$1) axes x1y2 with line linewidth 5 title 'Effective tax', \
    "<awk '{print $1, $2+$3-acc; acc=$2+$3}' tax-effective-and-perceived-2023-2024.dat" using 1:(100*$2/100) axes x1y2 with line title 'Perceived tax on last £100' linewidth 5
