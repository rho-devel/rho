expected <- eval(parse(text="TRUE"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(\"TRUE\", .Names = \".registration\"))"));  
do.call(`as.logical`, argv);  
}, o=expected);  

