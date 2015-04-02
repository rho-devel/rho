expected <- eval(parse(text="TRUE"));                   
test(id=0, code={                   
argv <- eval(parse(text="list(structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L), .Label = c(\"Ctl\", \"A\", \"B\"), class = \"factor\", contrasts = \"contr.treatment\"), \"factor\", FALSE)"));                   
.Internal(inherits(argv[[1]], argv[[2]], argv[[3]]));                   
}, o=expected);                   

