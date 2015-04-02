expected <- eval(parse(text="structure(c(0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 1L, 0L, 1L, 0L, 0L, 1L, 1L), .Dim = c(4L, 4L), .Dimnames = list(c(\"B\", \"V\", \"N\", \"V:N\"), c(\"Y\", \"B\", \"V\", \"N\")))"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(c(0L, 1L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 1L, 1L), .Dim = c(4L, 4L), .Dimnames = list(c(\"Y\", \"B\", \"V\", \"N\"), c(\"B\", \"V\", \"N\", \"V:N\"))))"));  
.Internal(`t.default`(argv[[1]]));  
}, o=expected);  

