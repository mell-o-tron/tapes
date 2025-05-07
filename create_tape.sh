if [ "$#" -ne 1 ]; then
    echo "Usage: bash $0 <figure-name>"
    exit 1
fi

cat ./tikz_out/prologue $1 ./tikz_out/epilogue > ./tikz_out/result.tex
rm ./figure.txt
pdflatex ./tikz_out/result.tex
rm ./result.aux
rm ./result.log 
