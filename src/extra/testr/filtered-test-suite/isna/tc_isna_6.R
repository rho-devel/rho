expected <- eval(parse(text="FALSE"));        
test(id=0, code={        
argv <- eval(parse(text="list(list(list(1)))"));        
do.call(`is.na`, argv);        
}, o=expected);        

