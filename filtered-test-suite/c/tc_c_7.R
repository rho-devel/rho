expected <- eval(parse(text="structure(expression(data.frame, check.names = TRUE, stringsAsFactors = TRUE), .Names = c(\"\", \"check.names\", \"stringsAsFactors\"))"));        
test(id=0, code={        
argv <- eval(parse(text="list(expression(data.frame), list(), check.names = TRUE, stringsAsFactors = TRUE)"));        
do.call(`c`, argv);        
}, o=expected);        

