expected <- eval(parse(text="\"function\""));              
test(id=0, code={              
argv <- eval(parse(text="list(.Primitive(\"dimnames<-\"))"));              
do.call(`class`, argv);              
}, o=expected);              

