expected <- eval(parse(text="structure(27.2746061113234, .Names = \"lymax\")"));            
test(id=0, code={            
argv <- eval(parse(text="list(structure(3.3059560902335, .Names = \"lymax\"))"));            
do.call(`exp`, argv);            
}, o=expected);            

