expected <- eval(parse(text="structure(TRUE, .Names = \"value\")"));              
test(id=0, code={              
argv <- eval(parse(text="list(structure(485.051413351662, .Names = \"value\"))"));              
do.call(`is.finite`, argv);              
}, o=expected);              

