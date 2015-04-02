expected <- eval(parse(text="structure(list(y = NULL, x1 = NULL, x2 = NULL, x3 = NULL), .Names = c(\"y\", \"x1\", \"x2\", \"x3\"), class = \"data.frame\", row.names = c(NA, 10L), terms = quote(y ~ x1 + x2 + x3))"));    
test(id=0, code={    
argv <- eval(parse(text="list(structure(list(y = c(2.30923841792462, 3.23011719303818, 2.9161246695212, 3.35931329373059, 5.3777049208621, 5.63518136825043, 7.37725908636056, 7.75621985157329, 10.1175871700049, 8.86877085545769), x1 = 1:10, x2 = 1:10, x3 = c(0.1, 0.4, 0.9, 1.6, 2.5, 3.6, 4.9, 6.4, 8.1, 10)), .Names = c(\"y\", \"x1\", \"x2\", \"x3\"), class = \"data.frame\", row.names = c(NA, 10L), terms = quote(y ~ x1 + x2 + x3)), structure(list(y = NULL, x1 = NULL, x2 = NULL, x3 = NULL), .Names = c(\"y\", \"x1\", \"x2\", \"x3\"), class = \"data.frame\", row.names = c(NA, 10L), terms = quote(y ~ x1 + x2 + x3)))"));    
.Internal(`copyDFattr`(argv[[1]], argv[[2]]));    
}, o=expected);    

