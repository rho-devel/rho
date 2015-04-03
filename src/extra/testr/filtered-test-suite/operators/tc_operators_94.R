expected <- eval(parse(text="TRUE"));          
test(id=0, code={          
argv <- eval(parse(text="list(\"a\", \"b\")"));          
do.call(`<`, argv);          
}, o=expected);          

