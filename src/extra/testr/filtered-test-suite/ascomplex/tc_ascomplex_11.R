expected <- eval(parse(text="complex(0)"));   
test(id=0, code={   
argv <- eval(parse(text="list(NULL, NULL)"));   
do.call(`as.complex`, argv);   
}, o=expected);   

