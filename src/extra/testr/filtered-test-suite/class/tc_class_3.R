expected <- eval(parse(text="structure(\"num1\", package = \".GlobalEnv\")"));              
test(id=0, code={              
argv <- eval(parse(text="list(structure(3.14159265358979, comment = \"Start with pi\", class = structure(\"num1\", package = \".GlobalEnv\")))"));              
do.call(`class`, argv);              
}, o=expected);              

