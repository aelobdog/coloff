html1='../index.html'
src1='index.sfl'
css1='web/style.css'

html2='lang.html'
src2='lang.sfl'
css2='style.css'

html_t='tmpl.html'
sfl -nts $html_t $css1 $src1 $html1 && echo "LOG : compiled $src1 to html."
sfl -nts $html_t $css2 $src2 $html2 && echo "LOG : compiled $src2 to html."
