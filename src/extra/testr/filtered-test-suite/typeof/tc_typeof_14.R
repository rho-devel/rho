expected <- eval(parse(text="\"raw\""));                
test(id=0, code={                
argv <- eval(parse(text="list(raw(0))"));                
.Internal(typeof(argv[[1]]));                
}, o=expected);                

