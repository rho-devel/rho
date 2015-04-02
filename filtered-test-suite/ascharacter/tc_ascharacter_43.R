expected <- eval(parse(text="\"NaN\""));        
test(id=0, code={        
argv <- eval(parse(text="list(NaN)"));        
do.call(`as.character`, argv);        
}, o=expected);        

