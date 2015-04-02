expected <- eval(parse(text="structure(0.272854377919402, .Names = \"Var1\")"));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(1.2728543779194, .Names = \"Var1\"), 1)"));               
do.call(`-`, argv);               
}, o=expected);               

