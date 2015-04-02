expected <- eval(parse(text="structure(c(FALSE, FALSE, TRUE), .Names = c(\"Depends\", \"Imports\", \"LinkingTo\"))"));                
test(id=0, code={                
argv <- eval(parse(text="list(structure(c(\"R (>= 2.10.0), methods, DBI (>= 0.2-5)\", \"methods, DBI (>= 0.2-3)\", NA), .Names = c(\"Depends\", \"Imports\", \"LinkingTo\")))"));                
do.call(`is.na`, argv);                
}, o=expected);                

