expected <- eval(parse(text="raw(0)"));   
test(id=0, code={   
argv <- eval(parse(text="list(list())"));   
do.call(`as.raw`, argv);   
}, o=expected);   

