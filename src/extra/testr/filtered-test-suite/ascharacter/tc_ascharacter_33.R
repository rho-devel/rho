expected <- eval(parse(text="character(0)"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(logical(0))"));                 
do.call(`as.character`, argv);                 
}, o=expected);                 

