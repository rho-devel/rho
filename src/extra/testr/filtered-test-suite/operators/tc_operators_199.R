expected <- eval(parse(text="0"));           
test(id=0, code={           
argv <- eval(parse(text="list(3e+09, 30000L)"));           
do.call(`%%`, argv);           
}, o=expected);           

