expected <- eval(parse(text="1:3"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(list(x = 1:3, y = structure(1:3, .Label = c(\"A\", \"D\", \"E\"), class = \"factor\"), z = c(6, 9, 10)), .Names = c(\"x\", \"y\", \"z\"), row.names = c(NA, -3L), class = \"data.frame\"))"));        
do.call(`seq_along`, argv);        
}, o=expected);        

