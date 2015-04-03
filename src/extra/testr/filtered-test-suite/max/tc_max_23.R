expected <- eval(parse(text="1"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(c(0, 1, 1, 1, 1), .Names = c(\"Hair\", \"Eye\", \"Sex\", \"Hair:Eye\", \"Hair:Sex\")))"));      
do.call(`max`, argv);      
}, o=expected);      

