expected <- eval(parse(text="c(\"-Inf\", \"NaN\", \"Inf\")"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(c(-Inf, NaN, Inf))"));                 
do.call(`as.character`, argv);                 
}, o=expected);                 

