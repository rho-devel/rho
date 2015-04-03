expected <- eval(parse(text="TRUE"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(list(structure(list(srcfile = c(\"/home/lzhao/hg/r-instrumented/library/methods/R/methods\", \"/home/lzhao/hg/r-instrumented/library/methods/R/methods\"), frow = c(6030L, 6032L), lrow = c(6031L, 6063L)), .Names = c(\"srcfile\", \"frow\", \"lrow\"), row.names = 1:2, class = \"data.frame\"), structure(list(srcfile = \"/home/lzhao/hg/r-instrumented/library/methods/R/methods\", frow = 6036L, lrow = 6055L), .Names = c(\"srcfile\", \"frow\", \"lrow\"), row.names = c(NA, -1L), class = \"data.frame\")), \"any\")"));                  
.Internal(is.vector(argv[[1]], argv[[2]]));                  
}, o=expected);                  

