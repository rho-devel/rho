expected <- eval(parse(text="FALSE"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(list(srcfile = c(NA, \"/home/lzhao/hg/r-instrumented/library/stats/R/stats\", \"/home/lzhao/hg/r-instrumented/library/stats/R/stats\"), frow = c(NA, 16987L, 16991L), lrow = c(NA, 16987L, 16991L)), .Names = c(\"srcfile\", \"frow\", \"lrow\"), row.names = c(NA, 3L), class = \"data.frame\"))"));                 
do.call(`is.matrix`, argv);                 
}, o=expected);                 

