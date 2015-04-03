expected <- eval(parse(text="structure(-0.0695860494901191, .Names = \"Var2\")"));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(-0.437222043740988, .Names = \"Var2\"), 6.28318530717959)"));               
do.call(`/`, argv);               
}, o=expected);               

