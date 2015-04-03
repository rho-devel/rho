expected <- eval(parse(text="structure(c(\"a\", \"b\", \"c\", \"d\", \"e\", \"f\", \"g\", \"h\", \"i\"), .Dim = c(3L, 3L), class = \"AsIs\")"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(list(z = structure(c(\"a\", \"b\", \"c\", \"d\", \"e\", \"f\", \"g\", \"h\", \"i\"), .Dim = c(3L, 3L), class = \"AsIs\")), .Names = \"z\"), 1L)"));                 
do.call(`.subset2`, argv);                 
}, o=expected);                 

