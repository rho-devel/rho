expected <- eval(parse(text="list(c(NA, 0L), c(NA, 0), c(10L, 10L), c(2.74035772634541, 2.74035772634541))"));       
test(id=0, code={       
argv <- eval(parse(text="list(list(structure(list(c(NA, 0L)), row.names = c(NA, -2L), class = \"data.frame\"), structure(list(c(NA, 0)), row.names = c(NA, -2L), class = \"data.frame\"), structure(list(c(10L, 10L)), row.names = c(NA, -2L), class = \"data.frame\"), structure(list(c(2.74035772634541, 2.74035772634541)), row.names = c(NA, -2L), class = \"data.frame\")), FALSE, FALSE)"));       
.Internal(`unlist`(argv[[1]], argv[[2]], argv[[3]]));       
}, o=expected);       

