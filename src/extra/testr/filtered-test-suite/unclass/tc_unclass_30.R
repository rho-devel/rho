expected <- eval(parse(text="structure(list(), .Names = character(0), row.names = c(NA, -10L), terms = quote(~0))"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(list(), .Names = character(0), row.names = c(NA, -10L), terms = quote(~0)))"));     
do.call(`unclass`, argv);     
}, o=expected);     

