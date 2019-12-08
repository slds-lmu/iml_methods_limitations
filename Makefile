all: html pdf

html: *.Rmd
	Rscript -e "bookdown::render_book('./', 'bookdown::gitbook')"

pdf: *.Rmd
	Rscript -e "bookdown::render_book('./', 'bookdown::pdf_book')"
