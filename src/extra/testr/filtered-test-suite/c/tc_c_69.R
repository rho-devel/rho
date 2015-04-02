expected <- eval(parse(text="structure(list(NA, FALSE, na.rm = TRUE), .Names = c(\"\", \"\", \"na.rm\"))"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(list(NA, FALSE), structure(list(na.rm = TRUE), .Names = \"na.rm\"))"));                  
do.call(`c`, argv);                  
}, o=expected);                  

