expected <- eval(parse(text="structure(2:3, .Label = c(\"C\", \"A\", \"B\"), class = \"factor\")"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(1:2, .Label = c(\"a\", \"b\"), class = \"factor\"), value = structure(list(C = \"C\", A = \"a\", B = \"b\"), .Names = c(\"C\", \"A\", \"B\")))"));  
do.call(`levels<-`, argv);  
}, o=expected);  

