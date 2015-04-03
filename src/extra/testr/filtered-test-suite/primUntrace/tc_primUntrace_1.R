expected <- eval(parse(text="NULL"));  
test(id=0, code={  
argv <- eval(parse(text="list(.Primitive(\"sum\"))"));  
do.call(`.primUntrace`, argv);  
}, o=expected);  

