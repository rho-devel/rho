expected <- eval(parse(text="structure(c(3, 8), .Dim = 2L, .Dimnames = structure(list(g = c(\"1\", \"2\")), .Names = \"g\"), call = quote(by.data.frame(data = X, INDICES = g, FUN = colMeans)), class = \"by\")"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(c(3, 8), .Dim = 2L, .Dimnames = structure(list(g = c(\"1\", \"2\")), .Names = \"g\"), call = quote(by.data.frame(data = X, INDICES = g, FUN = colMeans)), class = \"by\"))"));      
do.call(`invisible`, argv);      
}, o=expected);      

