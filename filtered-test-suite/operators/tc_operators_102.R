expected <- eval(parse(text="NA"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(integer(0), .Label = character(0), class = \"factor\"), 0L)"));             
do.call(`<=`, argv);             
}, o=expected);             

