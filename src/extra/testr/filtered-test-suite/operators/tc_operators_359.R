expected <- eval(parse(text="c(4L, 2L)"));                
test(id=0, code={                
argv <- eval(parse(text="list(c(2L, 1L), c(2L, 2L))"));                
do.call(`*`, argv);                
}, o=expected);                

