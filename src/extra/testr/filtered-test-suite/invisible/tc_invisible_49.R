expected <- eval(parse(text="structure(\"Error in read.table(\\\"foo1\\\") : no lines available in input\\n\", class = \"try-error\", condition = structure(list(message = \"no lines available in input\", call = quote(read.table(\"foo1\"))), .Names = c(\"message\", \"call\"), class = c(\"simpleError\", \"error\", \"condition\")))"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(\"Error in read.table(\\\"foo1\\\") : no lines available in input\\n\", class = \"try-error\", condition = structure(list(message = \"no lines available in input\", call = quote(read.table(\"foo1\"))), .Names = c(\"message\", \"call\"), class = c(\"simpleError\", \"error\", \"condition\"))))"));      
do.call(`invisible`, argv);      
}, o=expected);      

