expected <- eval(parse(text="structure(0, .Names = \"rate\")"));          
test(id=0, code={          
argv <- eval(parse(text="list(structure(1L, .Names = \"rate\"), 2)"));          
do.call(`%/%`, argv);          
}, o=expected);          

