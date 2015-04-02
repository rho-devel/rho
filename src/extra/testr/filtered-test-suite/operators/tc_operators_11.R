expected <- eval(parse(text="structure(104.660776108814, .Names = \"\")"));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(68.6851383798793, .Names = \"\"), structure(35.9756377289347, .Names = \"Var1\"))"));               
do.call(`+`, argv);               
}, o=expected);               

