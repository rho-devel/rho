expected <- eval(parse(text="list(structure(FALSE, .Tsp = c(1, 1, 1), class = \"ts\"), structure(FALSE, .Tsp = c(1, 1, 1), class = \"ts\"))"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(structure(FALSE, .Tsp = c(1, 1, 1), class = \"ts\"), structure(FALSE, .Tsp = c(1, 1, 1), class = \"ts\"))"));                  
do.call(`list`, argv);                  
}, o=expected);                  

