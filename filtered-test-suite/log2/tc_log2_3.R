expected <- eval(parse(text="-1022"));   
test(id=0, code={   
argv <- eval(parse(text="list(2.2250738585072e-308)"));   
do.call(`log2`, argv);   
}, o=expected);   

