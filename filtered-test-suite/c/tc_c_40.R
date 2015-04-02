expected <- eval(parse(text="structure(list(A = 1, c = \"C\", d1 = 1L, d2 = 2L, d3 = 3L), .Names = c(\"A\", \"c\", \"d1\", \"d2\", \"d3\"))"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(structure(list(A = 1, c = \"C\"), .Names = c(\"A\", \"c\")), d = 1:3)"));                  
do.call(`c`, argv);                  
}, o=expected);                  

