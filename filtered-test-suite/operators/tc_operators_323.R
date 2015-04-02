expected <- eval(parse(text="17"));          
test(id=0, code={          
argv <- eval(parse(text="list(34, 2L)"));          
do.call(`%/%`, argv);          
}, o=expected);          

