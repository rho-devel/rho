expected <- eval(parse(text="structure(c(11335, 11347, 11359, 11371, 11383, 11395, 11407, 11419, 11431), class = \"Date\")"));    
test(id=0, code={    
argv <- eval(parse(text="list(structure(11323, class = \"Date\"), c(12, 24, 36, 48, 60, 72, 84, 96, 108))"));    
do.call(`+`, argv);    
}, o=expected);    

