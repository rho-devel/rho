expected <- eval(parse(text="\"list\""));                
test(id=0, code={                
argv <- eval(parse(text="list(structure(list(a = 1), .Names = \"a\", .Tsp = c(1, 1, 1), class = \"ts\"))"));                
.Internal(typeof(argv[[1]]));                
}, o=expected);                

