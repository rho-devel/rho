expected <- eval(parse(text="NA_real_"));       
test(id=0, code={       
argv <- eval(parse(text="list(\"\", \"double\")"));       
.Internal(`as.vector`(argv[[1]], argv[[2]]));       
}, o=expected);       

