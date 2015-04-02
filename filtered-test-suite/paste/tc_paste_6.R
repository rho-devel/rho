expected <- eval(parse(text="\"dotplot\\rtable\\rNA\""));       
test(id=0, code={       
argv <- eval(parse(text="list(list(c(\"dotplot\", \"table\", NA)), \" \", \"\\r\")"));       
.Internal(`paste`(argv[[1]], argv[[2]], argv[[3]]));       
}, o=expected);       

