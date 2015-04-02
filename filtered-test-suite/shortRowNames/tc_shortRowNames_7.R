expected <- eval(parse(text="1L"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(list(srcfile = \"/home/lzhao/hg/r-instrumented/library/stats/R/stats\", frow = 5139L, lrow = 5139L), .Names = c(\"srcfile\", \"frow\", \"lrow\"), row.names = c(NA, -1L), class = \"data.frame\"), 2L)"));                 
.Internal(shortRowNames(argv[[1]], argv[[2]]));                 
}, o=expected);                 

