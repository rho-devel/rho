expected <- eval(parse(text="c(\"-4\", \"4\", \"3.99\", \"-1\", \"-3.01\")"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(c(-4, 4, 3.99, -1, -3.01))"));                 
do.call(`as.character`, argv);                 
}, o=expected);                 

