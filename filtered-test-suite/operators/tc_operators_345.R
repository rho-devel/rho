expected <- eval(parse(text=".Primitive(\"log\")"));               
test(id=0, code={               
argv <- eval(parse(text="list(.Primitive(\"log\"))"));               
do.call(`(`, argv);               
}, o=expected);               

