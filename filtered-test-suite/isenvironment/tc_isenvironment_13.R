expected <- eval(parse(text="FALSE"));          
test(id=0, code={          
argv <- eval(parse(text="list(structure(list(weight = c(1.9, 3.1, 3.3, 4.8, 5.3, 6.1, 6.4, 7.6, 9.8, 12.4), depression = c(2, 1, 5, 5, 20, 20, 23, 10, 30, 25)), .Names = c(\"weight\", \"depression\"), row.names = c(NA, -10L), class = \"data.frame\"))"));          
do.call(`is.environment`, argv);          
}, o=expected);          

