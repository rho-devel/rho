expected <- eval(parse(text="FALSE"));          
test(id=0, code={          
argv <- eval(parse(text="list(structure(list(coefficients = numeric(0), residuals = structure(c(-68.7898369431611, -71.7713382904347, -44.0000000000001, -56.5455568546283, -29.303772984227), .Dim = c(5L, 1L), .Dimnames = list(c(\"2\", \"3\", \"4\", \"5\", \"6\"), NULL)), fitted.values = structure(c(0, 0, 0, 0, 0), .Dim = c(5L, 1L), .Dimnames = list(c(\"2\", \"3\", \"4\", \"5\", \"6\"), NULL)), weights = NULL, rank = 0L, df.residual = 5L), .Names = c(\"coefficients\", \"residuals\", \"fitted.values\", \"weights\", \"rank\", \"df.residual\"), class = c(\"aov\", \"lm\")))"));          
do.call(`is.null`, argv);          
}, o=expected);          

