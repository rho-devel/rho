expected <- eval(parse(text="structure(c(1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L), class = \"factor\", .Label = c(\"A\", \"B\", \"C\", \"D\"))"));          
test(id=0, code={          
argv <- eval(parse(text="list(structure(1:4, .Label = c(\"A\", \"B\", \"C\", \"D\"), class = \"factor\", .Names = c(\"a\", \"b\", \"c\", \"d\")), 10)"));          
.Internal(rep_len(argv[[1]], argv[[2]]));          
}, o=expected);          

