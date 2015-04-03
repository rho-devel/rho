expected <- eval(parse(text="c(987.338461538462, 987.338461538462)"));  
test(id=0, code={  
argv <- eval(parse(text="list(987.338461538462, 2L)"));  
.Internal(`rep_len`(argv[[1]], argv[[2]]));  
}, o=expected);  

