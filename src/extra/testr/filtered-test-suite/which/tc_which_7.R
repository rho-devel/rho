expected <- eval(parse(text="2L"));            
test(id=0, code={            
argv <- eval(parse(text="list(c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))"));            
.Internal(which(argv[[1]]));            
}, o=expected);            

