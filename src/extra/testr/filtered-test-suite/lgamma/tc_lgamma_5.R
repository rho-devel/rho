expected <- eval(parse(text="Inf"));            
test(id=0, code={            
argv <- eval(parse(text="list(FALSE)"));            
do.call(`lgamma`, argv);            
}, o=expected);            

