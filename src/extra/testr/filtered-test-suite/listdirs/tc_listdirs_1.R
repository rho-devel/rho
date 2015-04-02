expected <- eval(parse(text="character(0)"));     
test(id=0, code={     
argv <- eval(parse(text="list(\"/home/lzhao/hg/r-instrumented/library/rpart/doc\", TRUE, FALSE)"));     
.Internal(list.dirs(argv[[1]], argv[[2]], argv[[3]]));     
}, o=expected);     

