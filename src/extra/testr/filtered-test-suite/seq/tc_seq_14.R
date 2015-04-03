expected <- eval(parse(text="1L"));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(list(20), row.names = c(NA, -1L)))"));               
do.call(`seq_along`, argv);               
}, o=expected);               

