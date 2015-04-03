expected <- eval(parse(text="structure(c(0.06, 0.32, 0.63, 909.591818181818, 0.905, 10000), .Names = c(\"0%\", \"25%\", \"50%\", \"\", \"75%\", \"100%\"))"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(c(0.06, 0.32, 0.63), .Names = c(\"0%\", \"25%\", \"50%\")), 909.591818181818, structure(c(0.905, 10000), .Names = c(\"75%\", \"100%\")))"));        
do.call(`c`, argv);        
}, o=expected);        

