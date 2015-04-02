expected <- eval(parse(text="structure(\"Error in cor(Z[, FALSE], use = \\\"pairwise.complete.obs\\\", method = \\\"kendall\\\") : \\n  'x' is empty\\n\", class = \"try-error\", condition = structure(list(message = \"'x' is empty\", call = quote(cor(Z[, FALSE], use = \"pairwise.complete.obs\", method = \"kendall\"))), .Names = c(\"message\", \"call\"), class = c(\"simpleError\", \"error\", \"condition\")))"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(\"Error in cor(Z[, FALSE], use = \\\"pairwise.complete.obs\\\", method = \\\"kendall\\\") : \\n  'x' is empty\\n\", class = \"try-error\", condition = structure(list(message = \"'x' is empty\", call = quote(cor(Z[, FALSE], use = \"pairwise.complete.obs\", method = \"kendall\"))), .Names = c(\"message\", \"call\"), class = c(\"simpleError\", \"error\", \"condition\"))))"));      
do.call(`invisible`, argv);      
}, o=expected);      

