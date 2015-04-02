expected <- eval(parse(text="structure(list(quote(list), V1 = quote(c(\"a\", \"d e\", \"h\")), V2 = quote(c(\"b'\", \"f\", \"i\")), V3 = quote(c(\"c\", \"g\", \"j\\nk l m\"))), .Names = c(\"\", \"V1\", \"V2\", \"V3\"))"));       
test(id=0, code={       
argv <- eval(parse(text="list(quote(list(V1 = c(\"a\", \"d e\", \"h\"), V2 = c(\"b'\", \"f\", \"i\"), V3 = c(\"c\", \"g\", \"j\\nk l m\"))), \"list\")"));       
.Internal(`as.vector`(argv[[1]], argv[[2]]));       
}, o=expected);       

