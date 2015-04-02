expected <- eval(parse(text="structure(\"Error in cov(rnorm(10), NULL) : \\n  supply both 'x' and 'y' or a matrix-like 'x'\\n\", class = \"try-error\", condition = structure(list(message = \"supply both 'x' and 'y' or a matrix-like 'x'\", call = quote(cov(rnorm(10), NULL))), .Names = c(\"message\", \"call\"), class = c(\"simpleError\", \"error\", \"condition\")))"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(\"Error in cov(rnorm(10), NULL) : \\n  supply both 'x' and 'y' or a matrix-like 'x'\\n\", class = \"try-error\", condition = structure(list(message = \"supply both 'x' and 'y' or a matrix-like 'x'\", call = quote(cov(rnorm(10), NULL))), .Names = c(\"message\", \"call\"), class = c(\"simpleError\", \"error\", \"condition\"))))"));      
do.call(`invisible`, argv);      
}, o=expected);      

