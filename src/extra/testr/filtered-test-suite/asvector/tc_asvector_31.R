expected <- eval(parse(text="list(quote(list), quote(cut(Dtimes, \"3 months\")))"));       
test(id=0, code={       
argv <- eval(parse(text="list(quote(list(cut(Dtimes, \"3 months\"))), \"list\")"));       
.Internal(`as.vector`(argv[[1]], argv[[2]]));       
}, o=expected);       

