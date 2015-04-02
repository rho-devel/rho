expected <- eval(parse(text="c(TRUE, TRUE)"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(c(13991, 13995), class = \"Date\"))"));      
do.call(`is.finite`, argv);      
}, o=expected);      

