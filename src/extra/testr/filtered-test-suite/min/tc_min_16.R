expected <- eval(parse(text="3"));     
test(id=0, code={     
argv <- eval(parse(text="list(3L, 7)"));     
do.call(`min`, argv);     
}, o=expected);     

