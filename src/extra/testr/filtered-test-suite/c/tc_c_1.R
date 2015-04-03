expected <- eval(parse(text="\"myLib/myTst\""));        
test(id=0, code={        
argv <- eval(parse(text="list(character(0), \"myLib/myTst\")"));        
do.call(`c`, argv);        
}, o=expected);        

