expected <- eval(parse(text="c(\"FALSE\", \"TRUE\")"));        
test(id=0, code={        
argv <- eval(parse(text="list(c(FALSE, TRUE))"));        
do.call(`as.character`, argv);        
}, o=expected);        

