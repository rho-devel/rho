expected <- eval(parse(text="c(\"   \", \"   \", \"   \", \"   \", \"   \", \"   \", \"   \", \"   \")"));            
test(id=0, code={            
argv <- eval(parse(text="list(\"   \", 8L)"));            
.Internal(rep.int(argv[[1]], argv[[2]]));            
}, o=expected);            

