expected <- eval(parse(text="structure(list(z = structure(c(\"a\", \"b\", \"c\", \"d\", \"e\", \"f\", \"g\", \"h\", \"i\"), .Dim = c(3L, 3L), class = \"AsIs\")), .Names = \"z\")"));                
test(id=0, code={                
argv <- eval(parse(text="list(structure(list(x = 1:3, y = structure(4:6, .Dim = c(3L, 1L), class = \"AsIs\"), z = structure(c(\"a\", \"b\", \"c\", \"d\", \"e\", \"f\", \"g\", \"h\", \"i\"), .Dim = c(3L, 3L), class = \"AsIs\")), .Names = c(\"x\", \"y\", \"z\"), row.names = c(NA, -3L), class = \"data.frame\"), \"z\")"));                
do.call(`.subset`, argv);                
}, o=expected);                

