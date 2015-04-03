expected <- eval(parse(text="structure(\"\\\\abc\\\\\", .Names = \"1\")"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(\"\\\\abc\\\\\", .Names = \"1\"), value = \"1\")"));  
do.call(`names<-`, argv);  
}, o=expected);  

