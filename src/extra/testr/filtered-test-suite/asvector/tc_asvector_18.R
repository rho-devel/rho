expected <- eval(parse(text="NA"));       
test(id=0, code={       
argv <- eval(parse(text="list(NA, \"logical\")"));       
.Internal(`as.vector`(argv[[1]], argv[[2]]));       
}, o=expected);       

