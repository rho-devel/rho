expected <- eval(parse(text="structure(list(`1` = NULL, `2` = NULL, `3` = NULL, `4` = NULL, `5` = NULL, `6` = NULL, `7` = NULL, `8` = NULL), .Dim = 8L, .Dimnames = list(c(\"1\", \"2\", \"3\", \"4\", \"5\", \"6\", \"7\", \"8\")))"));              
test(id=0, code={              
argv <- eval(parse(text="list(list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL), 8L, list(c(\"1\", \"2\", \"3\", \"4\", \"5\", \"6\", \"7\", \"8\")))"));              
.Internal(array(argv[[1]], argv[[2]], argv[[3]]));              
}, o=expected);              

