expected <- eval(parse(text="structure(c(1.00069354979237, 0.639336127879446, -0.0772037058662105), .Dim = c(1L, 3L))"));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(c(1.05569354979237, 0.694336127879446, -0.0222037058662105), .Dim = c(1L, 3L)), 0.055)"));               
do.call(`-`, argv);               
}, o=expected);               

