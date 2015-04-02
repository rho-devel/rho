expected <- eval(parse(text="\"Add Text to a Plot\""));  
test(id=0, code={  
argv <- eval(parse(text="list(\"Add Text to a Plot\")"));  
do.call(`enc2utf8`, argv);  
}, o=expected);  

