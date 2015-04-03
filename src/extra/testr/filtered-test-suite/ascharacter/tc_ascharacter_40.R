expected <- eval(parse(text="character(0)"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(list(), class = \"numeric_version\"))"));                 
do.call(`as.character`, argv);                 
}, o=expected);                 

