expected <- eval(parse(text="structure(c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), .Names = c(\"1\", \"2\", \"3\", \"4\", \"5\", \"6\", \"7\", \"8\", \"9\"))"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(c(69, 35, 26, 21, 18, 16, 13, 12, 12), .Names = c(\"1\", \"2\", \"3\", \"4\", \"5\", \"6\", \"7\", \"8\", \"9\")), 0)"));             
do.call(`<=`, argv);             
}, o=expected);             

