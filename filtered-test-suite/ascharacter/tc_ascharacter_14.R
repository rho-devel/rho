expected <- eval(parse(text="\"Estimates a probability density function,  \\n\""));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(\"Estimates a probability density function,  \\n\", Rd_tag = \"TEXT\"))"));                 
do.call(`as.character`, argv);                 
}, o=expected);                 

