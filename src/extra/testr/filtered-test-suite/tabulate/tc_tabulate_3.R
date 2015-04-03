expected <- eval(parse(text="0L"));  
test(id=0, code={  
argv <- eval(parse(text="list(integer(0), 1L)"));  
.Internal(`tabulate`(argv[[1]], argv[[2]]));  
}, o=expected);  

