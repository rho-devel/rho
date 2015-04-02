expected <- eval(parse(text="TRUE"));                
test(id=0, code={                
argv <- eval(parse(text="list(structure(c(0, -0.0555555555555556, 0.02, 0.0625, 0.0625, 0.04, 0, 0), .Dim = c(8L, 1L), .Dimnames = list(c(\"2\", \"3\", \"6\", \"7\", \"8\", \"9\", \"14\", \"17\"), \"x\")))"));                
do.call(`is.atomic`, argv);                
}, o=expected);                

