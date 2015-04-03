expected <- eval(parse(text="TRUE"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(\"(converted from warning) NAs produced\\n\", class = \"try-error\", condition = structure(list(message = \"(converted from warning) NAs produced\", call = quote(rnorm(1, sd = Inf))), .Names = c(\"message\", \"call\"), class = c(\"simpleError\", \"error\", \"condition\"))), \"(converted from warning) NAs produced\\n\")"));   
do.call(`==`, argv);   
}, o=expected);   

