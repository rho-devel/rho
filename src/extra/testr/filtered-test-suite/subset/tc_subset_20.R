expected <- eval(parse(text="structure(list(frow = 21911:21912), .Names = \"frow\")"));                
test(id=0, code={                
argv <- eval(parse(text="list(structure(list(srcfile = c(\"/home/lzhao/hg/r-instrumented/library/stats/R/stats\", \"/home/lzhao/hg/r-instrumented/library/stats/R/stats\"), frow = 21911:21912, lrow = 21911:21912), .Names = c(\"srcfile\", \"frow\", \"lrow\"), row.names = 1:2, class = \"data.frame\"), \"frow\")"));                
do.call(`.subset`, argv);                
}, o=expected);                

