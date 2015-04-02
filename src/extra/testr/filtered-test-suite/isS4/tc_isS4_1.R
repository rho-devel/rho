expected <- eval(parse(text="FALSE"));        
test(id=0, code={        
argv <- eval(parse(text="list(c(\"time\", \"status\"))"));        
do.call(`isS4`, argv);        
}, o=expected);        

