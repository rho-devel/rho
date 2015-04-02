expected <- eval(parse(text="1.234e+100"));             
test(id=0, code={             
argv <- eval(parse(text="list(1.234e+100)"));             
do.call(`min`, argv);             
}, o=expected);             

