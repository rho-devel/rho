expected <- eval(parse(text="0.0184459781655686"));   
test(id=0, code={   
argv <- eval(parse(text="list(-3.99290891786396)"));   
do.call(`exp`, argv);   
}, o=expected);   

