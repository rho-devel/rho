expected <- eval(parse(text="structure(NA_integer_, class = structure(\"foo\", package = \".GlobalEnv\"))"));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(FALSE, class = structure(\"foo\", package = \".GlobalEnv\")), NA)"));               
do.call(`+`, argv);               
}, o=expected);               

