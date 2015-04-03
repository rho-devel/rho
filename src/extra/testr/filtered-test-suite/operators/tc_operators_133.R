expected <- eval(parse(text="FALSE"));            
test(id=0, code={            
argv <- eval(parse(text="list(FALSE, \"Sweave\")"));            
do.call(`==`, argv);            
}, o=expected);            

