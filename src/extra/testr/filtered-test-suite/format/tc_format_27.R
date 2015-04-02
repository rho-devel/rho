expected <- eval(parse(text="c(\"       \", \"       \", \"\\\"Adult\\\"\", \"   \\\"No\\\"\", \"       \", \"    387\")"));            
test(id=0, code={            
argv <- eval(parse(text="list(c(\"\", \"\", \"\\\"Adult\\\"\", \"\\\"No\\\"\", \"\", \"387\"), FALSE, NULL, 0L, NULL, 1L, TRUE, NA)"));            
.Internal(format(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]]));            
}, o=expected);            

