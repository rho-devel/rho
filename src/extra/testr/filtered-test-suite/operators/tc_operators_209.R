expected <- eval(parse(text="NaN"));           
test(id=0, code={           
argv <- eval(parse(text="list(0, 0L)"));           
do.call(`%%`, argv);           
}, o=expected);           

