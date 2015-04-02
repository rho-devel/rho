expected <- eval(parse(text="75L"));                
test(id=0, code={                
argv <- eval(parse(text="list(\"> contour(x, y, volcano, levels = lev, col=\\\"yellow\\\", lty=\\\"solid\\\", add=TRUE)\", \"c\", FALSE)"));                
.Internal(nchar(argv[[1]], argv[[2]], argv[[3]]));                
}, o=expected);                

