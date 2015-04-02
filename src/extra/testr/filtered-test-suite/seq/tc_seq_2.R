expected <- eval(parse(text="1:6"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(c(1, 2, 3, 0, 10, NA), .Dim = c(3L, 2L)))"));        
do.call(`seq_along`, argv);        
}, o=expected);        

