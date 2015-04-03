expected <- eval(parse(text="c(\"simpleCondition\", \"condition\")"));              
test(id=0, code={              
argv <- eval(parse(text="list(structure(list(message = \"Choosing method ‘sparseMatrix#ANY’ from 2 ambiguous possibilities\", call = NULL), .Names = c(\"message\", \"call\"), class = c(\"simpleCondition\", \"condition\")))"));              
do.call(`class`, argv);              
}, o=expected);              

