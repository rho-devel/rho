expected <- eval(parse(text="FALSE"));         
test(id=0, code={         
argv <- eval(parse(text="list(quote(breaks ~ (wool + tension) - tension), \"formula\", FALSE)"));         
.Internal(`inherits`(argv[[1]], argv[[2]], argv[[3]]));         
}, o=expected);         

