expected <- eval(parse(text="c(\"3\", \"3\", NA, NA, NA, NA, \"4\", \"3\", \"4\", NA, NA, \"2\", \"3\", \"3\", NA, NA, \"2\", \"4\", NA, \"2\", \"5\", \"2\", \"2\", \"4\", \"3\", NA, \"2\", NA, \"3\", \"3\")"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(c(3, 3, NA, NA, NA, NA, 4, 3, 4, NA, NA, 2, 3, 3, NA, NA, 2, 4, NA, 2, 5, 2, 2, 4, 3, NA, 2, NA, 3, 3))"));                 
do.call(`as.character`, argv);                 
}, o=expected);                 

