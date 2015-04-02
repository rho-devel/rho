expected <- eval(parse(text="list(structure(\"Math2\", package = \"methods\"), \"round\", \"signif\")"));      
test(id=0, code={      
argv <- eval(parse(text="list(list(structure(\"Math2\", package = \"methods\"), \"round\", \"signif\"), FALSE, FALSE, NA)"));      
.Internal(`unique`(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));      
}, o=expected);      

