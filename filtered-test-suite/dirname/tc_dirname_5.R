expected <- eval(parse(text="\"/home/lzhao/hg/r-instrumented/library\""));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(\"/home/lzhao/hg/r-instrumented/library/utils\", .Names = \"Dir\"))"));        
.Internal(dirname(argv[[1]]));        
}, o=expected);        

