expected <- eval(parse(text="\"/home/lzhao/hg/r-instrumented/library/translations\""));        
test(id=0, code={        
argv <- eval(parse(text="list(\"utils\", \"/home/lzhao/hg/r-instrumented/library/translations\")"));        
.Internal(bindtextdomain(argv[[1]], argv[[2]]));        
}, o=expected);        

