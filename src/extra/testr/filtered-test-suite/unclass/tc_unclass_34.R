expected <- eval(parse(text="structure(list(group = structure(c(1L, 1L), .Label = c(\"Ctl\", \"Trt\"), class = \"factor\")), .Names = \"group\", row.names = 1:2, terms = quote(~group))"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(list(group = structure(c(1L, 1L), .Label = c(\"Ctl\", \"Trt\"), class = \"factor\")), .Names = \"group\", row.names = 1:2, terms = quote(~group)))"));     
do.call(`unclass`, argv);     
}, o=expected);     

