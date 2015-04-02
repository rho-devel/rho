expected <- eval(parse(text="TRUE"));                   
test(id=0, code={                   
argv <- eval(parse(text="list(structure(list(srcfile = \"/home/lzhao/hg/r-instrumented/library/stats/R/stats\", frow = 853L, lrow = 853L), .Names = c(\"srcfile\", \"frow\", \"lrow\"), row.names = c(NA, -1L), class = \"data.frame\"), \"data.frame\", FALSE)"));                   
.Internal(inherits(argv[[1]], argv[[2]], argv[[3]]));                   
}, o=expected);                   

