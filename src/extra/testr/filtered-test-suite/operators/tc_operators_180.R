expected <- eval(parse(text="structure(NA_real_, .Dim = c(1L, 1L))"));      
test(id=0, code={      
argv <- eval(parse(text="list(0, NA_real_)"));      
do.call(`%*%`, argv);      
}, o=expected);      

