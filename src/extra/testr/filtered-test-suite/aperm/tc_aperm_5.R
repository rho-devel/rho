expected <- eval(parse(text="structure(c(9.2319289524956, -0.470372045488369, 186.857050189827), .Dim = c(1L, 3L), .Dimnames = list(\"118\", c(\"age\", \"sex\", \"meal.cal\")))"));           
test(id=0, code={           
argv <- eval(parse(text="list(structure(c(9.2319289524956, -0.470372045488369, 186.857050189827), .Dim = c(1L, 3L), .Dimnames = list(\"118\", c(\"age\", \"sex\", \"meal.cal\"))), 1:2, TRUE)"));           
.Internal(aperm(argv[[1]], argv[[2]], argv[[3]]));           
}, o=expected);           

