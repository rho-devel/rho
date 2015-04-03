expected <- eval(parse(text="\"list\""));                
test(id=0, code={                
argv <- eval(parse(text="list(structure(list(c0 = logical(0)), .Names = \"c0\", row.names = integer(0)))"));                
.Internal(typeof(argv[[1]]));                
}, o=expected);                

