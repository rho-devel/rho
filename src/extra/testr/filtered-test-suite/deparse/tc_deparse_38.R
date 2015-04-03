expected <- eval(parse(text="\"structure(list(c0 = structure(integer(0), .Label = character(0), class = \\\"factor\\\")), .Names = \\\"c0\\\", row.names = character(0), class = structure(\\\"integer(0)\\\", .Names = \\\"c0\\\"))\""));          
test(id=0, code={          
argv <- eval(parse(text="list(structure(list(c0 = structure(integer(0), .Label = character(0), class = \"factor\")), .Names = \"c0\", row.names = character(0), class = structure(\"integer(0)\", .Names = \"c0\")), 60L, FALSE, 69, -1L)"));          
.Internal(deparse(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));          
}, o=expected);          

