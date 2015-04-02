expected <- eval(parse(text="c(TRUE, TRUE, TRUE, TRUE, TRUE)"));             
test(id=0, code={             
argv <- eval(parse(text="list(c(\"Fr\", \"Temp\", \"Soft\", \"M.user\", \"Brand\"))"));             
do.call(`nzchar`, argv);             
}, o=expected);             

