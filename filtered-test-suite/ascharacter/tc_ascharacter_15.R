expected <- eval(parse(text="c(\"2\", \"1\", \"3\", NA, \"4\")"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(c(2L, 1L, 3L, NA, 4L))"));                 
do.call(`as.character`, argv);                 
}, o=expected);                 

