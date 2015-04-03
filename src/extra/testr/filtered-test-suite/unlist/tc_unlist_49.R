expected <- eval(parse(text="list(1L, structure(1L, .Label = c(\"A\", \"D\", \"E\"), class = \"factor\"), 6)"));       
test(id=0, code={       
argv <- eval(parse(text="list(list(structure(list(x = 1L, y = structure(1L, .Label = c(\"A\", \"D\", \"E\"), class = \"factor\"), z = 6), .Names = c(\"x\", \"y\", \"z\"), row.names = 1L, class = \"data.frame\"), structure(list(), .Names = character(0), row.names = 1L, class = \"data.frame\")), FALSE, FALSE)"));       
.Internal(`unlist`(argv[[1]], argv[[2]], argv[[3]]));       
}, o=expected);       

