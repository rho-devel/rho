expected <- eval(parse(text="\"list\""));                
test(id=0, code={                
argv <- eval(parse(text="list(structure(list(x = structure(1L, .Label = \"1.3\", class = \"factor\")), .Names = \"x\", row.names = c(NA, -1L), class = \"data.frame\"))"));                
.Internal(typeof(argv[[1]]));                
}, o=expected);                

