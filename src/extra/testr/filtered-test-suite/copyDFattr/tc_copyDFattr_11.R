expected <- eval(parse(text="structure(list(srcfile = NULL, frow = NULL, lrow = NULL), .Names = c(\"srcfile\", \"frow\", \"lrow\"), row.names = c(NA, 4L), class = \"data.frame\")"));              
test(id=0, code={              
argv <- eval(parse(text="list(structure(list(srcfile = c(NA, \"/home/lzhao/hg/r-instrumented/library/stats/R/stats\", \"/home/lzhao/hg/r-instrumented/library/stats/R/stats\", \"/home/lzhao/hg/r-instrumented/library/stats/R/stats\"), frow = c(NA, 2228L, 2369L, 2379L), lrow = c(NA, 2228L, 2369L, 2380L)), .Names = c(\"srcfile\", \"frow\", \"lrow\"), row.names = c(NA, 4L), class = \"data.frame\"), structure(list(srcfile = NULL, frow = NULL, lrow = NULL), .Names = c(\"srcfile\", \"frow\", \"lrow\"), row.names = c(NA, 4L), class = \"data.frame\"))"));              
.Internal(copyDFattr(argv[[1]], argv[[2]]));              
}, o=expected);              

