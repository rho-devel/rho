expected <- eval(parse(text="2147483648"));       
test(id=0, code={       
argv <- eval(parse(text="list(1073741824L, 1073741824L, 0)"));       
do.call(`sum`, argv);       
}, o=expected);       

