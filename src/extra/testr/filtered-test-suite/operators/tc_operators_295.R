expected <- eval(parse(text="structure(c(1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2), .Dim = 3:4, .Dimnames = list(c(\"Case_1\", \"Case_2\", \"Case_3\"), NULL))"));           
test(id=0, code={           
argv <- eval(parse(text="list(structure(1:12, .Dim = 3:4, .Dimnames = list(c(\"Case_1\", \"Case_2\", \"Case_3\"), NULL)), 5)"));           
do.call(`%%`, argv);           
}, o=expected);           

