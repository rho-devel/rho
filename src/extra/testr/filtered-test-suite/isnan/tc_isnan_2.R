expected <- eval(parse(text="structure(c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), .Names = c(\"a1\", \"a2\", \"a3\", \"a4\", \"a5\", \"a6\", \"a7\"))"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(1:7, .Names = c(\"a1\", \"a2\", \"a3\", \"a4\", \"a5\", \"a6\", \"a7\")))"));   
do.call(`is.nan`, argv);   
}, o=expected);   

