expected <- eval(parse(text="1:2"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(c(3, 8), .Dim = 2L, .Dimnames = structure(list(g = c(\"1\", \"2\")), .Names = \"g\"), call = quote(by.data.frame(data = X, INDICES = g, FUN = colMeans)), class = \"by\"))"));        
do.call(`seq_along`, argv);        
}, o=expected);        

