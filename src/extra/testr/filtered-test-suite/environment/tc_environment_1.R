expected <- eval(parse(text="NULL"));  
test(id=0, code={  
argv <- eval(parse(text="list(quote(cbind(X, M) ~ M.user + Temp + M.user:Temp + Soft))"));  
.Internal(`environment`(argv[[1]]));  
}, o=expected);  

