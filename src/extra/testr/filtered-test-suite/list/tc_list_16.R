expected <- eval(parse(text="structure(list(aa = structure(c(\"1\", \"2\", \"3\"), class = \"AsIs\"), ..dfd.row.names = structure(c(\"4\", \"5\", \"6\", \"7\", \"8\", \"9\"), .Dim = c(3L, 2L), .Dimnames = list(NULL, c(\"a\", \"b\")))), .Names = c(\"aa\", \"..dfd.row.names\"))"));         
test(id=0, code={         
argv <- eval(parse(text="list(aa = structure(c(\"1\", \"2\", \"3\"), class = \"AsIs\"), ..dfd.row.names = structure(c(\"4\", \"5\", \"6\", \"7\", \"8\", \"9\"), .Dim = c(3L, 2L), .Dimnames = list(NULL, c(\"a\", \"b\"))))"));         
do.call(`list`, argv);         
}, o=expected);         

