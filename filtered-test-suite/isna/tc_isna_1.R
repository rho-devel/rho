expected <- eval(parse(text="FALSE"));        
test(id=0, code={        
argv <- eval(parse(text="list(8.21977282218514e-09)"));        
do.call(`is.na`, argv);        
}, o=expected);        

