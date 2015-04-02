expected <- eval(parse(text="NULL"));      
test(id=0, code={      
argv <- eval(parse(text="list(\"Error in validObject(.Object) : \\n  invalid class “trackCurve” object: Unequal x,y lengths: 20, 10\\n\")"));      
.Internal(seterrmessage(argv[[1]]));      
}, o=expected);      

