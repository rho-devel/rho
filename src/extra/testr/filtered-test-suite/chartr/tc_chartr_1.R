expected <- eval(parse(text="c(\"0.02\", \"0.06\", \"0.11\", \"0.22\", \"0.56\", \"1.1\")"));    
test(id=0, code={    
argv <- eval(parse(text="list(\".\", \".\", c(\"0.02\", \"0.06\", \"0.11\", \"0.22\", \"0.56\", \"1.1\"))"));    
.Internal(chartr(argv[[1]], argv[[2]], argv[[3]]));    
}, o=expected);    

