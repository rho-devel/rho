expected <- eval(parse(text="c(\"-0.32+0.00i\", \"0.00-0.56i\", \"1.00+0.00i\", \"0.00+1.77i\", \"-3.14+0.00i\")"));            
test(id=0, code={            
argv <- eval(parse(text="list(c(-0.318309886183791+0i, 0-0.564189583547756i, 1+0i, 0+1.77245385090552i, -3.14159265358979+0i), TRUE, 2, 0L, NULL, 3L, TRUE, NA)"));            
.Internal(format(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]]));            
}, o=expected);            

