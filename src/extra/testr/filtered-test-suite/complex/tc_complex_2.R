expected <- eval(parse(text="0+0i"));  
test(id=0, code={  
argv <- eval(parse(text="list(FALSE, FALSE, numeric(0))"));  
.Internal(complex(argv[[1]], argv[[2]], argv[[3]]));  
}, o=expected);  

