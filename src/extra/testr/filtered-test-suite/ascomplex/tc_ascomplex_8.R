expected <- eval(parse(text="complex(0)"));   
test(id=0, code={   
argv <- eval(parse(text="list(integer(0))"));   
do.call(`as.complex`, argv);   
}, o=expected);   

