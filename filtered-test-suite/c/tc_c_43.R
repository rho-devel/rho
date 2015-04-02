expected <- eval(parse(text="structure(list(structure(list(u = c(5, 10, 15, 20, 30, 40, 60, 80, 100), lot1 = c(118, 58, 42, 35, 27, 25, 21, 19, 18), lot2 = c(69, 35, 26, 21, 18, 16, 13, 12, 12)), .Names = c(\"u\", \"lot1\", \"lot2\"), row.names = c(NA, -9L), class = \"data.frame\"), max.level = 0, give.attr = FALSE, digits = 3), .Names = c(\"\", \"max.level\", \"give.attr\", \"digits\"))"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(list(structure(list(u = c(5, 10, 15, 20, 30, 40, 60, 80, 100), lot1 = c(118, 58, 42, 35, 27, 25, 21, 19, 18), lot2 = c(69, 35, 26, 21, 18, 16, 13, 12, 12)), .Names = c(\"u\", \"lot1\", \"lot2\"), row.names = c(NA, -9L), class = \"data.frame\")), structure(list(max.level = 0, give.attr = FALSE, digits = 3), .Names = c(\"max.level\", \"give.attr\", \"digits\")))"));                  
do.call(`c`, argv);                  
}, o=expected);                  

