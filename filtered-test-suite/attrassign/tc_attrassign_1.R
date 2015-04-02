expected <- eval(parse(text="structure(1, foo = structure(list(a = \"a\"), .Names = \"a\"))"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(1, foo = structure(list(a = \"a\"), .Names = \"a\")), \"foo\", value = structure(list(a = \"a\"), .Names = \"a\"))"));  
do.call(`attr<-`, argv);  
}, o=expected);  

