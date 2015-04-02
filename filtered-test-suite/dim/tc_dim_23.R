expected <- eval(parse(text="NULL"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(1:20, .Tsp = c(1960.08333333333, 1961.66666666667, 12), class = \"ts\"))"));       
do.call(`dim`, argv);       
}, o=expected);       

