expected <- eval(parse(text="list(quote(list), quote(ff <- factor(c(1:2, NA, 2), exclude = NULL)))"));       
test(id=0, code={       
argv <- eval(parse(text="list(quote(list(ff <- factor(c(1:2, NA, 2), exclude = NULL))), \"list\")"));       
.Internal(`as.vector`(argv[[1]], argv[[2]]));       
}, o=expected);       

