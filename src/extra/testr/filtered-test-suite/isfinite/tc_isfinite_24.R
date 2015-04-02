expected <- eval(parse(text="c(TRUE, TRUE)"));      
test(id=0, code={      
argv <- eval(parse(text="list(c(13L, 13L))"));      
do.call(`is.finite`, argv);      
}, o=expected);      

