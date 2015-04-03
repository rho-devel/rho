expected <- eval(parse(text="\"1967-09-13\""));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(-841, class = \"Date\"))"));                 
do.call(`as.character`, argv);                 
}, o=expected);                 

