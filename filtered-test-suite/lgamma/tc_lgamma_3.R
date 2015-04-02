expected <- eval(parse(text="-0.0893685018986132"));            
test(id=0, code={            
argv <- eval(parse(text="list(1.72926007700446)"));            
do.call(`lgamma`, argv);            
}, o=expected);            

