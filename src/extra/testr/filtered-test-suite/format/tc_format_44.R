expected <- eval(parse(text="\"-0.01234000000000+3.14159265358979i\""));            
test(id=0, code={            
argv <- eval(parse(text="list(-0.01234+3.14159265358979i, FALSE, NULL, 14L, NULL, 3L, TRUE, NA)"));            
.Internal(format(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]]));            
}, o=expected);            

