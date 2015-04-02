expected <- eval(parse(text="structure(list(Depends = structure(logical(0), .Dim = c(0L, 3L)), Installed = structure(logical(0), .Dim = c(0L, 3L)), R = structure(logical(0), .Dim = c(0L, 3L))), .Names = c(\"Depends\", \"Installed\", \"R\"))"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(Depends = structure(logical(0), .Dim = c(0L, 3L)), Installed = structure(logical(0), .Dim = c(0L, 3L)), R = structure(logical(0), .Dim = c(0L, 3L)))"));                  
do.call(`list`, argv);                  
}, o=expected);                  

