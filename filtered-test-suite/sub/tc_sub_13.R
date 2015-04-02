expected <- eval(parse(text="structure(\"(converted from warning) NAs produced\\n\", class = \"try-error\", condition = structure(list(message = \"(converted from warning) NAs produced\", call = quote(rexp(2, numeric()))), .Names = c(\"message\", \"call\"), class = c(\"simpleError\", \"error\", \"condition\")))"));   
test(id=0, code={   
argv <- eval(parse(text="list(\".* : \", \"\", structure(\"Error in rexp(2, numeric()) : (converted from warning) NAs produced\\n\", class = \"try-error\", condition = structure(list(message = \"(converted from warning) NAs produced\", call = quote(rexp(2, numeric()))), .Names = c(\"message\", \"call\"), class = c(\"simpleError\", \"error\", \"condition\"))), FALSE, FALSE, FALSE, FALSE)"));   
.Internal(`sub`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));   
}, o=expected);   

