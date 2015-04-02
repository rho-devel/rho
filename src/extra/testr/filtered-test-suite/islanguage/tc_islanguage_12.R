expected <- eval(parse(text="TRUE"));    
test(id=0, code={    
argv <- eval(parse(text="list(expression(sqrt(abs(`Standardized residuals`))))"));    
do.call(`is.language`, argv);    
}, o=expected);    

