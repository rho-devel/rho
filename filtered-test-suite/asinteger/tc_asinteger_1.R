expected <- eval(parse(text="c(4L, 5L, 3L, 2L, 2L, 1L, 6L)"));    
test(id=0, code={    
argv <- eval(parse(text="list(structure(c(4L, 5L, 3L, 2L, 2L, 1L, 6L), .Label = c(\"McNeil\", \"Ripley\", \"Tierney\", \"Tukey\", \"Venables\", \"R Core\"), class = \"factor\"))"));    
do.call(`as.integer`, argv);    
}, o=expected);    

