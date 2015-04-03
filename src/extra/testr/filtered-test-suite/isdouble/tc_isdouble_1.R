expected <- eval(parse(text="FALSE"));   
test(id=0, code={   
argv <- eval(parse(text="list(list(1, list(3, \"A\")))"));   
do.call(`is.double`, argv);   
}, o=expected);   

