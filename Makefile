file=index

book:
	Rscript -e 'library(bookdown);bookdown::render_book("$(file).Rmd", "all")'

rm_main:
	rm _main.Rmd

rm_bookfiles:
	rm -rf _bookdown_files

htmlopen:
	qutebrowser _book/$(file).html &

rsync_book:
	rsync -avzhe ssh --info=progress2 --delete _book/ bibr@159.89.36.185:/var/www/roneyfraga.com/public_html/projects/2022-climate-change/

all:
	Rscript -e 'library(bookdown);bookdown::render_book("$(file).Rmd", "all")'
	rsync -avzhe "ssh -i ~/.chave/chave_limpa" --info=progress2 --delete _book/ bibr@159.89.36.185:/var/www/roneyfraga.com/public_html/projects/2022-climate-change/
