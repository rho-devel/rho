expected <- eval(parse(text="6:10"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(list(a = 6:10), .Names = \"a\", row.names = 6:10), FALSE, FALSE)"));       
.Internal(`unlist`(argv[[1]], argv[[2]], argv[[3]]));       
}, o=expected);       

