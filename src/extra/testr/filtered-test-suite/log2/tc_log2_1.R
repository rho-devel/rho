expected <- eval(parse(text="5.58496250072116"));   
test(id=0, code={   
argv <- eval(parse(text="list(48L)"));   
do.call(`log2`, argv);   
}, o=expected);   

