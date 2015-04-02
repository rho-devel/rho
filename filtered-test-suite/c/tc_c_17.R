expected <- eval(parse(text="1:10"));        
test(id=0, code={        
argv <- eval(parse(text="list(logical(0), structure(1:10, .Tsp = c(1920.5, 1921.25, 12), class = \"ts\"), logical(0))"));        
do.call(`c`, argv);        
}, o=expected);        

