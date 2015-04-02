expected <- eval(parse(text="3.72109932186642e-05"));            
test(id=0, code={            
argv <- eval(parse(text="list(0.999935539560166)"));            
do.call(`lgamma`, argv);            
}, o=expected);            

