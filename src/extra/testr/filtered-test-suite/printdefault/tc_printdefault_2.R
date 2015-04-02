expected <- eval(parse(text="structure(c(\" 1\", \"NA\", \" 1\", \"1.1\", \" NA\", \"2.0\", \"1.1+0i\", \"    NA\", \"3.0+0i\", \"NA\", \"NA\", \"NA\", \"FALSE\", \"   NA\", \" TRUE\", \"abc\", NA, \"def\"), .Dim = c(3L, 6L), .Dimnames = list(c(\"1\", \"2\", \"3\"), c(\"A\", \"B\", \"C\", \"D\", \"E\", \"F\")))"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(c(\" 1\", \"NA\", \" 1\", \"1.1\", \" NA\", \"2.0\", \"1.1+0i\", \"    NA\", \"3.0+0i\", \"NA\", \"NA\", \"NA\", \"FALSE\", \"   NA\", \" TRUE\", \"abc\", NA, \"def\"), .Dim = c(3L, 6L), .Dimnames = list(c(\"1\", \"2\", \"3\"), c(\"A\", \"B\", \"C\", \"D\", \"E\", \"F\"))), NULL, FALSE, NULL, NULL, TRUE, NULL, TRUE, FALSE)"));     
.Internal(`print.default`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]], argv[[9]]));     
}, o=expected);     

