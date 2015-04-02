expected <- eval(parse(text="\"/home/lzhao/hg/r-instrumented/tests/Packages/rpart/R/summary.rpart.R\""));            
test(id=0, code={            
argv <- eval(parse(text="list(list(\"/home/lzhao/hg/r-instrumented/tests/Packages/rpart/R\", \"summary.rpart.R\"), \"/\")"));            
.Internal(file.path(argv[[1]], argv[[2]]));            
}, o=expected);            

