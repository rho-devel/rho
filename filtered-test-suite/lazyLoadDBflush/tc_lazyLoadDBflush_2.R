expected <- eval(parse(text="NULL"));   
test(id=0, code={   
argv <- eval(parse(text="list(\"/home/lzhao/hg/r-instrumented/library/stats4/R/stats4.rdb\")"));   
.Internal(lazyLoadDBflush(argv[[1]]));   
}, o=expected);   

