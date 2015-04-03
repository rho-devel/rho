expected <- eval(parse(text="0"));           
test(id=0, code={           
argv <- eval(parse(text="list(16L, 8)"));           
do.call(`%%`, argv);           
}, o=expected);           

