expected <- eval(parse(text="1:11"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(list(A = 0:10, B = 10:20, `NA` = 20:30), .Names = c(\"A\", \"B\", NA), row.names = c(NA, -11L), class = \"data.frame\"), \"row.names\")"));        
do.call(`attr`, argv);        
}, o=expected);        

