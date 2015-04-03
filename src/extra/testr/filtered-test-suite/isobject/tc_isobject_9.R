expected <- eval(parse(text="FALSE"));  
test(id=0, code={  
argv <- eval(parse(text="list(c(FALSE, FALSE))"));  
do.call(`is.object`, argv);  
}, o=expected);  

