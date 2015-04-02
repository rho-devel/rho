expected <- eval(parse(text="FALSE"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(c(1960, 1960, 1960, 1960, 1960, 1960, 1960, 1960, 1960, 1960, 1960, 1961, 1961, 1961, 1961, 1961, 1961, 1961, 1961, 1961), .Tsp = c(1960.08333333333, 1961.66666666667, 12), class = \"ts\"))"));       
do.call(`is.call`, argv);       
}, o=expected);       

