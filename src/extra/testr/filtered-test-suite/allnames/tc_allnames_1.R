expected <- eval(parse(text="c(\"y\", \"g1\", \"g2\", \"k\", \"x\", \"Ta\", \"Tb\")"));  
test(id=0, code={  
argv <- eval(parse(text="list(quote(y ~ ((g1) * exp((log(g2/g1)) * (1 - exp(-k * (x - Ta)))/(1 - exp(-k * (Tb - Ta)))))), FALSE, -1L, TRUE)"));  
.Internal(`all.names`(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));  
}, o=expected);  

