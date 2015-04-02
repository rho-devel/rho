expected <- eval(parse(text="structure(list(Var1 = c(1L, 2L, 3L, 0L, 1L, 2L, 0L, 1L, 0L)), .Names = \"Var1\")"));                
test(id=0, code={                
argv <- eval(parse(text="list(structure(list(Var1 = c(1L, 2L, 3L, 0L, 1L, 2L, 0L, 1L, 0L), Var2 = c(0L, 0L, 0L, 1L, 1L, 1L, 2L, 2L, 3L)), .Names = c(\"Var1\", \"Var2\"), out.attrs = structure(list(dim = c(4L, 4L), dimnames = structure(list(Var1 = c(\"Var1=0\", \"Var1=1\", \"Var1=2\", \"Var1=3\"), Var2 = c(\"Var2=0\", \"Var2=1\", \"Var2=2\", \"Var2=3\")), .Names = c(\"Var1\", \"Var2\"))), .Names = c(\"dim\", \"dimnames\")), row.names = c(2L, 3L, 4L, 5L, 6L, 7L, 9L, 10L, 13L), class = \"data.frame\"), 1)"));                
do.call(`.subset`, argv);                
}, o=expected);                

