expected <- eval(parse(text="1"));              
test(id=0, code={              
argv <- eval(parse(text="list(1, 2)"));              
do.call(`%%`, argv);              
}, o=expected);              

