expected <- eval(parse(text="structure(list(class = \"try-error\", condition = structure(list(message = \"non-numeric argument to mathematical function\", call = quote(log(\"a\"))), .Names = c(\"message\", \"call\"), class = c(\"simpleError\", \"error\", \"condition\"))), .Names = c(\"class\", \"condition\"))"));        
test(id=0, code={        
argv <- eval(parse(text="list(NULL, structure(list(class = \"try-error\", condition = structure(list(message = \"non-numeric argument to mathematical function\", call = quote(log(\"a\"))), .Names = c(\"message\", \"call\"), class = c(\"simpleError\", \"error\", \"condition\"))), .Names = c(\"class\", \"condition\")))"));        
do.call(`c`, argv);        
}, o=expected);        

