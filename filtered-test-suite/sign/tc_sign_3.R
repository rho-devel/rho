expected <- eval(parse(text="structure(-1, .Names = \"W\")"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(-29.5, .Names = \"W\"))"));  
do.call(`sign`, argv);  
}, o=expected);  

