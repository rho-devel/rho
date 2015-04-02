expected <- eval(parse(text="\"double\""));         
test(id=0, code={         
argv <- eval(parse(text="list(structure(c(1960, 1960, 1960, 1960, 1960, 1960, 1960, 1960, 1960, 1960, 1960, 1961, 1961, 1961, 1961, 1961, 1961, 1961, 1961, 1961), .Tsp = c(1960.08333333333, 1961.66666666667, 12), class = \"ts\"))"));         
.Internal(`typeof`(argv[[1]]));         
}, o=expected);         

