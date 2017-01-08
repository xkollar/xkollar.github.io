INFILE=data/images/Ouroboros-linear.svg
OUTFILE=data/images/favicon.ico
convert \
    -density 1200 \
    -background transparent \
    "${INFILE}" \
    -gravity center \
    -scale 500x500^ \
    -extent 500x500 \
    -bordercolor none \
    -border 6 \
    -define icon:auto-resize=16,32,48,64,128,256 \
    -compress zip \
    "${OUTFILE}"
