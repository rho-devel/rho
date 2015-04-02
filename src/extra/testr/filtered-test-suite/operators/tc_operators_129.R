expected <- eval(parse(text="TRUE"));            
test(id=0, code={            
argv <- eval(parse(text="list(2, structure(list(2L), class = structure(\"L\", package = \".GlobalEnv\")))"));            
do.call(`==`, argv);            
}, o=expected);            

