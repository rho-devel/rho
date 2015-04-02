expected <- eval(parse(text="-7"));     
test(id=0, code={     
argv <- eval(parse(text="list(c(-7, -5.6, -4.2, -2.8, -1.4, 0, 1.4, 2.8, 4.2, 5.6, 7))"));     
do.call(`min`, argv);     
}, o=expected);     

