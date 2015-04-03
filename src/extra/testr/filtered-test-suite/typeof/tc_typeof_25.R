expected <- eval(parse(text="\"double\""));                
test(id=0, code={                
argv <- eval(parse(text="list(structure(3.14159265358979, comment = \"Start with pi\", class = structure(\"num1\", package = \".GlobalEnv\")))"));                
.Internal(typeof(argv[[1]]));                
}, o=expected);                

