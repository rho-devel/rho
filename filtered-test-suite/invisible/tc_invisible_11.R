expected <- eval(parse(text="structure(\"Error in `[.data.frame`(dd, , \\\"x\\\") : undefined columns selected\\n\", class = \"try-error\", condition = structure(list(message = \"undefined columns selected\", call = quote(`[.data.frame`(dd, , \"x\"))), .Names = c(\"message\", \"call\"), class = c(\"simpleError\", \"error\", \"condition\")))"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(\"Error in `[.data.frame`(dd, , \\\"x\\\") : undefined columns selected\\n\", class = \"try-error\", condition = structure(list(message = \"undefined columns selected\", call = quote(`[.data.frame`(dd, , \"x\"))), .Names = c(\"message\", \"call\"), class = c(\"simpleError\", \"error\", \"condition\"))))"));      
do.call(`invisible`, argv);      
}, o=expected);      

