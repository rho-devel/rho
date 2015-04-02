expected <- eval(parse(text="0L"));  
test(id=0, code={  
argv <- eval(parse(text="list(character(0), FALSE, FALSE)"));  
.Internal(`unlink`(argv[[1]], argv[[2]], argv[[3]]));  
}, o=expected);  

