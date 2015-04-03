expected <- eval(parse(text="structure(c(\" 16\", \" 16\", \"144\", \" 16\", \" 16\", \"128\", \" 16\", \" 16\", \"112\", \" 16\"), .Dim = 10L, .Dimnames = structure(list(c(\"1\", \"6\", \"7\", \"8\", \"13\", \"14\", \"15\", \"20\", \"21\", \"22\")), .Names = \"\"))"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(c(\" 16\", \" 16\", \"144\", \" 16\", \" 16\", \"128\", \" 16\", \" 16\", \"112\", \" 16\"), .Dim = 10L, .Dimnames = structure(list(c(\"1\", \"6\", \"7\", \"8\", \"13\", \"14\", \"15\", \"20\", \"21\", \"22\")), .Names = \"\")))"));             
.Internal(drop(argv[[1]]));             
}, o=expected);             

