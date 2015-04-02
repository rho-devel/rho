expected <- eval(parse(text="0+0i"));   
test(id=0, code={   
argv <- eval(parse(text="list(FALSE, FALSE)"));   
do.call(`as.complex`, argv);   
}, o=expected);   

