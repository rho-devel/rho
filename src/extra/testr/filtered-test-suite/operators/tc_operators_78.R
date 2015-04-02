expected <- eval(parse(text="TRUE"));          
test(id=0, code={          
argv <- eval(parse(text="list(1, 4)"));          
do.call(`<`, argv);          
}, o=expected);          

