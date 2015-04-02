expected <- eval(parse(text="1:4"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L), .Label = c(\"Brown\", \"Blue\", \"Hazel\", \"Green\"), class = \"factor\"), FALSE, FALSE, 5L)"));      
.Internal(`unique`(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));      
}, o=expected);      

