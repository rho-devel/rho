expected <- eval(parse(text="-20:10"));     
test(id=0, code={     
argv <- eval(parse(text="list(-20, 10)"));     
do.call(`:`, argv);     
}, o=expected);     

