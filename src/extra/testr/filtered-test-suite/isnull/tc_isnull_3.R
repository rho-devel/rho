expected <- eval(parse(text="FALSE"));          
test(id=0, code={          
argv <- eval(parse(text="list(structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L), .Label = c(\"L\", \"M\", \"H\"), class = \"factor\"))"));          
do.call(`is.null`, argv);          
}, o=expected);          

