expected <- eval(parse(text="FALSE"));            
test(id=0, code={            
argv <- eval(parse(text="list(\"/home/lzhao/hg/r-instrumented/library/methods/data/Rdata.rdb\")"));            
.Internal(file.exists(argv[[1]]));            
}, o=expected);            

