expected <- eval(parse(text="structure(c(1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0), .Dim = 3:4)"));           
test(id=0, code={           
argv <- eval(parse(text="list(structure(1:12, .Dim = 3:4), 3)"));           
do.call(`%%`, argv);           
}, o=expected);           

