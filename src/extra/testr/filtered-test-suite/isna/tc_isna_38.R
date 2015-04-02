expected <- eval(parse(text="c(FALSE, FALSE)"));                
test(id=0, code={                
argv <- eval(parse(text="list(structure(list(1L, 3L), class = structure(\"L\", package = \".GlobalEnv\")))"));                
do.call(`is.na`, argv);                
}, o=expected);                

