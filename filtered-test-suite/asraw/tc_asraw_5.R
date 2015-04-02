expected <- eval(parse(text="raw(0)"));   
test(id=0, code={   
argv <- eval(parse(text="list(NULL)"));   
do.call(`as.raw`, argv);   
}, o=expected);   

