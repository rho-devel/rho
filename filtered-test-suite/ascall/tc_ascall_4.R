expected <- eval(parse(text="quote(data.frame(check.names = TRUE, stringsAsFactors = TRUE))"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(expression(data.frame, check.names = TRUE, stringsAsFactors = TRUE), .Names = c(\"\", \"check.names\", \"stringsAsFactors\")))"));   
do.call(`as.call`, argv);   
}, o=expected);   

