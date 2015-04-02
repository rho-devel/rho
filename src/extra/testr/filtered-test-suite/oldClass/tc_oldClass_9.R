expected <- eval(parse(text="\"factor\""));                
test(id=0, code={                
argv <- eval(parse(text="list(structure(c(4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L), .Label = c(\"C\", \"E\", \"D\", \"A\", \"F\", \"B\"), class = \"factor\", scores = structure(c(14, 16.5, 1.5, 5, 3, 15), .Dim = 6L, .Dimnames = list(c(\"A\", \"B\", \"C\", \"D\", \"E\", \"F\")))))"));                
do.call(`oldClass`, argv);                
}, o=expected);                

