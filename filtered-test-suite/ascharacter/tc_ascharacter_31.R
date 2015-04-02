expected <- eval(parse(text="c(\"2012-06-01\", \"2012-06-02\", \"2012-06-03\", \"2012-06-04\", \"2012-06-05\", \"2012-06-06\", \"2012-06-07\")"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(c(1338523200, 1338609600, 1338696000, 1338782400, 1338868800, 1338955200, 1339041600), class = c(\"POSIXct\", \"POSIXt\"), tzone = \"\"))"));                 
do.call(`as.character`, argv);                 
}, o=expected);                 

