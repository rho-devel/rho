expected <- eval(parse(text="TRUE"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(\"MASS\", .Names = \"\"))"));      
do.call(`nzchar`, argv);      
}, o=expected);      

