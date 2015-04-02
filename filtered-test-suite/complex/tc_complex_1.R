expected <- eval(parse(text="complex(0)"));  
test(id=0, code={  
argv <- eval(parse(text="list(0, numeric(0), numeric(0))"));  
.Internal(`complex`(argv[[1]], argv[[2]], argv[[3]]));  
}, o=expected);  

