expected <- eval(parse(text="structure(0, .Names = \"data\")"));               
test(id=0, code={               
argv <- eval(parse(text="list(1979.91666666667, structure(1979.91666666667, .Names = \"data\"))"));               
do.call(`-`, argv);               
}, o=expected);               

