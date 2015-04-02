expected <- eval(parse(text="TRUE"));   
test(id=0, code={   
argv <- eval(parse(text="list(\"pch\")"));   
do.call(`is.character`, argv);   
}, o=expected);   

