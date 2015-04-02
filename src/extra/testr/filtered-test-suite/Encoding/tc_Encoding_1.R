expected <- eval(parse(text="\"unknown\""));                
test(id=0, code={                
argv <- eval(parse(text="list(\"Byte Code Compiler\")"));                
.Internal(Encoding(argv[[1]]));                
}, o=expected);                

