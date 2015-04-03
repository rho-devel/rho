expected <- eval(parse(text="FALSE"));         
test(id=0, code={         
argv <- eval(parse(text="list(\"FALSE\")"));         
do.call(`as.logical`, argv);         
}, o=expected);         

