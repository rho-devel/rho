expected <- eval(parse(text="structure(list(value = structure(c(NA, NA, 1L, 9L), .Names = c(\"size\", \"current\", \"direction\", \"eval_depth\")), visible = TRUE), .Names = c(\"value\", \"visible\"))"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(list(value = structure(c(NA, NA, 1L, 9L), .Names = c(\"size\", \"current\", \"direction\", \"eval_depth\")), visible = TRUE), .Names = c(\"value\", \"visible\")))"));      
do.call(`invisible`, argv);      
}, o=expected);      

