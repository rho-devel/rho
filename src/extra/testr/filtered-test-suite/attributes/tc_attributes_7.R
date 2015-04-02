expected <- eval(parse(text="NULL"));               
test(id=0, code={               
argv <- eval(parse(text="list(NULL)"));               
do.call(`attributes`, argv);               
}, o=expected);               

