expected <- eval(parse(text="TRUE"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(list(structure(list(srcfile = c(NA, \"/home/lzhao/hg/r-instrumented/library/graphics/R/graphics\"), frow = c(NA, 3990L), lrow = c(NA, 3991L)), .Names = c(\"srcfile\", \"frow\", \"lrow\"), row.names = 1:2, class = \"data.frame\"), structure(list(srcfile = \"/home/lzhao/hg/r-instrumented/library/graphics/R/graphics\", frow = 3998L, lrow = 4009L), .Names = c(\"srcfile\", \"frow\", \"lrow\"), row.names = c(NA, -1L), class = \"data.frame\")), \"any\")"));                  
.Internal(is.vector(argv[[1]], argv[[2]]));                  
}, o=expected);                  

