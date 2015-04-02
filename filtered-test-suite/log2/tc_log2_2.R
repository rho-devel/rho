expected <- eval(parse(text="-Inf"));   
test(id=0, code={   
argv <- eval(parse(text="list(FALSE)"));   
do.call(`log2`, argv);   
}, o=expected);   

