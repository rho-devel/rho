expected <- eval(parse(text="3162.27766016838"));            
test(id=0, code={            
argv <- eval(parse(text="list(1e+07)"));            
do.call(`sqrt`, argv);            
}, o=expected);            

