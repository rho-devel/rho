expected <- eval(parse(text="302.184407485412"));   
test(id=0, code={   
argv <- eval(parse(text="list(1.529e+302)"));   
do.call(`log10`, argv);   
}, o=expected);   

