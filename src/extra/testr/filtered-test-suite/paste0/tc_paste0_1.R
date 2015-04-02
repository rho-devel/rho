expected <- eval(parse(text="\"2: \""));       
test(id=0, code={       
argv <- eval(parse(text="list(list(\"2\", \": \"), NULL)"));       
.Internal(`paste0`(argv[[1]], argv[[2]]));       
}, o=expected);       

