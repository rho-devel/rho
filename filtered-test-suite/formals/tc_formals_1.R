expected <- eval(parse(text="NULL"));      
test(id=0, code={      
argv <- eval(parse(text="list(.Primitive(\"length<-\"))"));      
.Internal(formals(argv[[1]]));      
}, o=expected);      

