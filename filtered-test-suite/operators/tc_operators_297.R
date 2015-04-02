expected <- eval(parse(text="logical(0)"));              
test(id=0, code={              
argv <- eval(parse(text="list(structure(integer(0), .Label = character(0), class = \"factor\"))"));              
do.call(`^`, argv);              
}, o=expected);              

