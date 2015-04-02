expected <- eval(parse(text="NULL"));   
test(id=0, code={   
argv <- eval(parse(text="list(\"incomplete final line found by readTableHeader on 'foo4'\", quote(read.table(\"foo4\", header = TRUE)))"));   
.Internal(`.dfltWarn`(argv[[1]], argv[[2]]));   
}, o=expected);   

