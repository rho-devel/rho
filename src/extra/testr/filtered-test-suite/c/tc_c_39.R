expected <- eval(parse(text="structure(list(c(1L, 2L, 4L), 1:3, c(2L, 1L)), class = c(\"package_version\", \"numeric_version\"))"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(structure(list(c(1L, 2L, 4L), 1:3, c(2L, 1L)), class = c(\"package_version\", \"numeric_version\")))"));                  
do.call(`c`, argv);                  
}, o=expected);                  

