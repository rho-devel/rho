expected <- eval(parse(text="TRUE"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(\"survival\", .Names = \"\"))"));             
do.call(`nzchar`, argv);             
}, o=expected);             

