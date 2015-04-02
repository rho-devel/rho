expected <- eval(parse(text="structure(list(names = structure(\"stats\", .Names = \"name\")), .Names = \"names\")"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(NULL, structure(list(names = structure(\"stats\", .Names = \"name\")), .Names = \"names\"))"));                  
do.call(`c`, argv);                  
}, o=expected);                  

