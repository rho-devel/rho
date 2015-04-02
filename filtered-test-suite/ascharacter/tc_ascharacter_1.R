expected <- eval(parse(text="\"bessel_y(2,nu=181.2): precision lost in result\""));        
test(id=0, code={        
argv <- eval(parse(text="list(\"bessel_y(2,nu=181.2): precision lost in result\")"));        
do.call(`as.character`, argv);        
}, o=expected);        

