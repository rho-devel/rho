expected <- eval(parse(text="FALSE"));          
test(id=0, code={          
argv <- eval(parse(text="list(structure(c(NA, NA, 159.125, 204, 221.25, 245.125, 319.75, 451.5, 561.125, 619.25, 615.625, 548, 462.125, 381.125, 316.625, 264, 228.375, 210.75, 188.375, 199, 207.125, 191, 166.875, 72, -9.25, -33.125, -36.75, 36.25, 103, 131.625, NA, NA), .Tsp = c(1951, 1958.75, 4), class = \"ts\"))"));          
do.call(`is.null`, argv);          
}, o=expected);          

