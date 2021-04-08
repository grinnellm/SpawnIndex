# Clean up the directory after a LaTeX build (Windows version)
del *.aux
del *.bbl
del *.blg
del *.dvi
del *.log
del *.lof
del *.lot
del *.nav
del *.out
del *.ps
del *.snm
del *.toc
del *.upa
del *.vrb
del *-concordance.tex
del *.synctex.gz*
del SpawnIndex.tex
del SpawnIndex.pdf
rmdir /S /Q knitr-cache
rmdir /S /Q knitr-figs
rmdir /S /Q cache
