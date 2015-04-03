expected <- eval(parse(text="c(\"file55711ba85492.R\", \"file55711ba85492.R\")"));  
test(id=0, code={  
argv <- eval(parse(text="list(c(\"file55711ba85492.R\", \"/file55711ba85492.R\"))"));  
.Internal(`basename`(argv[[1]]));  
}, o=expected);  

