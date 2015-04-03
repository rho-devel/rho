expected <- eval(parse(text="c(1, 1, 0)"));     
test(id=0, code={     
argv <- eval(parse(text="list(c(TRUE, TRUE, FALSE))"));     
do.call(`as.double`, argv);     
}, o=expected);     

