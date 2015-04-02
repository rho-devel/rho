expected <- eval(parse(text="10"));           
test(id=0, code={           
argv <- eval(parse(text="list(10, 16L)"));           
do.call(`%%`, argv);           
}, o=expected);           

