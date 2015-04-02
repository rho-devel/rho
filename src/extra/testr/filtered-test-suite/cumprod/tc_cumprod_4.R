expected <- eval(parse(text="structure(0, .Names = \"l0\")"));          
test(id=0, code={          
argv <- eval(parse(text="list(structure(0L, .Names = \"l0\"))"));          
do.call(`cumprod`, argv);          
}, o=expected);          

