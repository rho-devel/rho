expected <- eval(parse(text="c(FALSE, FALSE, FALSE)"));  
test(id=0, code={  
argv <- eval(parse(text="list(1:3)"));  
do.call(`is.nan`, argv);  
}, o=expected);  

