expected <- eval(parse(text="NULL"));               
test(id=0, code={               
argv <- eval(parse(text="list(1386518010.66723)"));               
do.call(`attributes`, argv);               
}, o=expected);               

