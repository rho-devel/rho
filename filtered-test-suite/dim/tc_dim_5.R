expected <- eval(parse(text="c(1L, 5L)"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(list(df = 10L, ss = 2.74035772634541, ms = 0.274035772634541, f = NA_real_, P = NA_real_), .Names = c(\"df\", \"ss\", \"ms\", \"f\", \"P\"), row.names = c(NA, -1L), class = \"data.frame\"))"));       
do.call(`dim`, argv);       
}, o=expected);       

