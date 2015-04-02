expected <- eval(parse(text="complex(0)"));   
test(id=0, code={   
argv <- eval(parse(text="list(logical(0), logical(0))"));   
do.call(`as.complex`, argv);   
}, o=expected);   

