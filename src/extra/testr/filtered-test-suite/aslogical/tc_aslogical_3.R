expected <- eval(parse(text="c(TRUE, FALSE, FALSE, FALSE, FALSE)"));         
test(id=0, code={         
argv <- eval(parse(text="list(structure(c(1L, 0L, 0L, 0L, 0L), .Names = c(\"bibtype\", NA, NA, NA, NA)))"));         
do.call(`as.logical`, argv);         
}, o=expected);         

