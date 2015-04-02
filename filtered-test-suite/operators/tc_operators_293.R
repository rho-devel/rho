expected <- eval(parse(text="structure(NA_real_, .Names = \"power\")"));              
test(id=0, code={              
argv <- eval(parse(text="list(NA_integer_, structure(-2, .Names = \"power\"))"));              
do.call(`^`, argv);              
}, o=expected);              

