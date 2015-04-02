expected <- eval(parse(text="\"\\\\method{as.dist}{default}\""));               
test(id=0, code={               
argv <- eval(parse(text="list(\"\\\"\", \"\\\\\\\"\", \"\\\\method{as.dist}{default}\", FALSE, FALSE, TRUE, FALSE)"));               
.Internal(sub(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));               
}, o=expected);               

