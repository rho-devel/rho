expected <- eval(parse(text="FALSE"));    
test(id=0, code={    
argv <- eval(parse(text="list(structure(list(weight = c(4.17, 5.58, 5.18, 6.11, 4.5, 4.61, 5.17, 4.53, 5.33, 5.14, 4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69), group = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L), .Label = c(\"Ctl\", \"Trt\"), class = \"factor\")), .Names = c(\"weight\", \"group\"), row.names = c(NA, -20L), class = \"data.frame\"))"));    
do.call(`is.array`, argv);    
}, o=expected);    

