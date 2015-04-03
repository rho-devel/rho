expected <- eval(parse(text="structure(c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), .Names = c(\"1\", \"2\", \"3\", \"4\", \"5\", \"6\"))"));          
test(id=0, code={          
argv <- eval(parse(text="list(structure(c(629, 1026, 1422, 1819, 2214, 2611), class = \"difftime\", units = \"days\", .Names = c(\"1\", \"2\", \"3\", \"4\", \"5\", \"6\")), 0)"));          
do.call(`<`, argv);          
}, o=expected);          

