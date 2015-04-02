expected <- eval(parse(text="TRUE"));                   
test(id=0, code={                   
argv <- eval(parse(text="list(structure(list(srcfile = c(\"/home/lzhao/hg/r-instrumented/library/grid/R/grid\", \"/home/lzhao/hg/r-instrumented/library/grid/R/grid\"), frow = 3581:3582, lrow = c(3581L, 3590L)), .Names = c(\"srcfile\", \"frow\", \"lrow\"), row.names = 1:2, class = \"data.frame\"), \"data.frame\", FALSE)"));                   
.Internal(inherits(argv[[1]], argv[[2]], argv[[3]]));                   
}, o=expected);                   

