expected <- eval(parse(text="50"));          
test(id=0, code={          
argv <- eval(parse(text="list(350L, 7)"));          
do.call(`%/%`, argv);          
}, o=expected);          

