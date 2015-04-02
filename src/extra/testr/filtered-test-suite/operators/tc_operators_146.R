expected <- eval(parse(text="structure(FALSE, .Tsp = c(1, 1, 1), class = \"ts\")"));           
test(id=0, code={           
argv <- eval(parse(text="list(structure(0, .Tsp = c(1, 1, 1), class = \"ts\"), 1e-05)"));           
do.call(`>`, argv);           
}, o=expected);           

