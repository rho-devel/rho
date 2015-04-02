expected <- eval(parse(text="c(FALSE, FALSE, FALSE)"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(1:3, .Label = c(\"1\", \"2\", NA), class = \"factor\"), structure(1:3, .Label = c(\"1\", \"2\", NA), class = \"factor\"))"));  
do.call(`!=`, argv);  
}, o=expected);  

