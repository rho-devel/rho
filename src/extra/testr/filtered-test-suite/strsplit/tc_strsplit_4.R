expected <- eval(parse(text="list(c(\"Keywords:\", \"\", \"device\"))"));              
test(id=0, code={              
argv <- eval(parse(text="list(\"Keywords:  device \", \"[ \\t\\n]\", FALSE, TRUE, TRUE)"));              
.Internal(strsplit(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));              
}, o=expected);              

