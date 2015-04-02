expected <- eval(parse(text=".Primitive(\"c\")"));   
test(id=0, code={   
argv <- eval(parse(text="list(\"c\")"));   
do.call(`.Primitive`, argv);   
}, o=expected);   

