expected <- eval(parse(text="raw(0)"));   
test(id=0, code={   
argv <- eval(parse(text="list(logical(0))"));   
do.call(`as.raw`, argv);   
}, o=expected);   

