expected <- eval(parse(text="c(FALSE, FALSE)"));            
test(id=0, code={            
argv <- eval(parse(text="list(c(\"src/Makevars\", \"src/Makevars.in\"))"));            
.Internal(file.exists(argv[[1]]));            
}, o=expected);            

