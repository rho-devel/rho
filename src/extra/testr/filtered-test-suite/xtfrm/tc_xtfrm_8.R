expected <- eval(parse(text="1:3"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(1:3, id = \"An Example\", class = structure(\"numWithId\", package = \".GlobalEnv\")))"));     
do.call(`xtfrm`, argv);     
}, o=expected);     

