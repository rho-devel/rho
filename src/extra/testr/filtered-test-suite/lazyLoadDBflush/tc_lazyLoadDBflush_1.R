expected <- eval(parse(text="NULL"));  
test(id=0, code={  
argv <- eval(parse(text="list(\"/home/roman/r-instrumented/library/tools/R/tools.rdb\")"));  
.Internal(`lazyLoadDBflush`(argv[[1]]));  
}, o=expected);  

