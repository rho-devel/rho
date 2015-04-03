expected <- eval(parse(text="c(TRUE, TRUE, TRUE)"));      
test(id=0, code={      
argv <- eval(parse(text="list(c(\"a\", \"b\", \"c\"))"));      
do.call(`nzchar`, argv);      
}, o=expected);      

