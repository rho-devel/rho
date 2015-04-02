expected <- eval(parse(text="\"tk_messageBox.Rd\""));            
test(id=0, code={            
argv <- eval(parse(text="list(\"tk_messageBox.Rd\")"));            
.Internal(basename(argv[[1]]));            
}, o=expected);            

