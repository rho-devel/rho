expected <- eval(parse(text="0"));         
test(id=0, code={         
argv <- eval(parse(text="list(-0.698970004336019)"));         
do.call(`ceiling`, argv);         
}, o=expected);         

