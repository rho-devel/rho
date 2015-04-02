expected <- eval(parse(text="c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11)"));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(1:10, class = structure(\"NumericNotStructure\", package = \".GlobalEnv\")), 1)"));               
do.call(`+`, argv);               
}, o=expected);               

