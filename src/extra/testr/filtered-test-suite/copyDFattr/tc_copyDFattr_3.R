expected <- eval(parse(text="structure(list(y = NULL, x = NULL, z = NULL), .Names = c(\"y\", \"x\", \"z\"), class = \"data.frame\", row.names = c(NA, 10L), terms = quote(y ~ x * z))"));    
test(id=0, code={    
argv <- eval(parse(text="list(structure(list(y = c(-0.0561287395290008, -0.155795506705329, -1.47075238389927, -0.47815005510862, 0.417941560199702, 1.35867955152904, -0.102787727342996, 0.387671611559369, -0.0538050405829051, -1.37705955682861), x = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE), z = 1:10), .Names = c(\"y\", \"x\", \"z\"), class = \"data.frame\", row.names = c(NA, 10L), terms = quote(y ~ x * z)), structure(list(y = NULL, x = NULL, z = NULL), .Names = c(\"y\", \"x\", \"z\"), class = \"data.frame\", row.names = c(NA, 10L), terms = quote(y ~ x * z)))"));    
.Internal(`copyDFattr`(argv[[1]], argv[[2]]));    
}, o=expected);    

