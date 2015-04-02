expected <- eval(parse(text="structure(list(x = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)), .Names = \"x\", row.names = c(NA, 10L))"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(list(x = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)), .Names = \"x\", row.names = c(NA, 10L)))"));     
do.call(`unclass`, argv);     
}, o=expected);     

