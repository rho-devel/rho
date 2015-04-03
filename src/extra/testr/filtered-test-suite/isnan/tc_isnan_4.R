expected <- eval(parse(text="structure(TRUE, .Dim = c(1L, 1L))"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(NaN, .Dim = c(1L, 1L)))"));  
do.call(`is.nan`, argv);  
}, o=expected);  

