expected <- eval(parse(text="\"utils\""));            
test(id=0, code={            
argv <- eval(parse(text="list(structure(\"/home/lzhao/hg/r-instrumented/library/utils\", .Names = \"Dir\"))"));            
.Internal(basename(argv[[1]]));            
}, o=expected);            

