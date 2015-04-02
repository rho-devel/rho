expected <- eval(parse(text="50:51"));     
test(id=0, code={     
argv <- eval(parse(text="list(50, 51)"));     
do.call(`:`, argv);     
}, o=expected);     

