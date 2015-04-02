expected <- eval(parse(text="structure(0.0741357767596835, .Names = \"Var1\")"));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(0.070387338608913, .Names = \"Var1\"), structure(0.00374843815077052, .Names = \"Var2\"))"));               
do.call(`+`, argv);               
}, o=expected);               

