expected <- eval(parse(text="NULL"));           
test(id=0, code={           
argv <- eval(parse(text="list(.Primitive(\"/\"))"));           
.Internal(body(argv[[1]]));           
}, o=expected);           

