

html: *.Rmd
	Rscript -e "bookdown::render_book('./', 'bookdown::gitbook')"

