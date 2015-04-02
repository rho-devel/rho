expected <- eval(parse(text="c(64, 93, 215, 220)"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(c(5, 29, 14, 16, 15, 54, 14, 10, 20, 84, 17, 94, 68, 119, 26, 7), .Dim = c(4L, 4L), .Dimnames = structure(list(Hair = c(\"Black\", \"Brown\", \"Red\", \"Blond\"), Eye = c(\"Green\", \"Hazel\", \"Blue\", \"Brown\")), .Names = c(\"Hair\", \"Eye\"))), 4, 4, FALSE)"));        
.Internal(colSums(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));        
}, o=expected);        

