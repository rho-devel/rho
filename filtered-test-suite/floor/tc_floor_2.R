expected <- eval(parse(text="structure(c(12784, 13149, 13514, 13879, 14245, 14610), tzone = \"UTC\")"));    
test(id=0, code={    
argv <- eval(parse(text="list(structure(c(12784, 13149, 13514, 13879, 14245, 14610), tzone = \"UTC\"))"));    
do.call(`floor`, argv);    
}, o=expected);    

