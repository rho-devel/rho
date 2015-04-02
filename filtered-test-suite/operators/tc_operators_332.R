expected <- eval(parse(text="structure(c(TRUE, TRUE), .Names = c(\"(Intercept)\", \"age\"))"));          
test(id=0, code={          
argv <- eval(parse(text="list(structure(c(FALSE, FALSE), .Names = c(\"(Intercept)\", \"age\")))"));          
do.call(`!`, argv);          
}, o=expected);          

