expected <- eval(parse(text="c(\"numeric\", \"vector\", \"atomicVector\")"));   
test(id=0, code={   
argv <- eval(parse(text="list(\"numeric\", c(\"numeric\", \"vector\", \"atomicVector\"))"));   
do.call(`.cache_class`, argv);   
}, o=expected);   

