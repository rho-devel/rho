expected <- eval(parse(text="\"data.frame\""));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(list(), .Names = character(0), class = \"data.frame\", row.names = c(NA, -10L)))"));      
do.call(`oldClass`, argv);      
}, o=expected);      

