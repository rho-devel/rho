expected <- eval(parse(text="expression(sqrt(abs(`Standardized residuals`)))"));       
test(id=0, code={       
argv <- eval(parse(text="list(quote(sqrt(abs(`Standardized residuals`))), \"expression\")"));       
.Internal(`as.vector`(argv[[1]], argv[[2]]));       
}, o=expected);       

