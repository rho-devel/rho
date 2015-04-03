expected <- eval(parse(text="1:2"));           
test(id=0, code={           
argv <- eval(parse(text="list(1:2, 4L)"));           
do.call(`%%`, argv);           
}, o=expected);           

