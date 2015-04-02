expected <- eval(parse(text="c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0)"));           
test(id=0, code={           
argv <- eval(parse(text="list(-1:12, 2)"));           
do.call(`%%`, argv);           
}, o=expected);           

