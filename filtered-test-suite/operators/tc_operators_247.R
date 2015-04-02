expected <- eval(parse(text="-7:-1"));     
test(id=0, code={     
argv <- eval(parse(text="list(-7L, -1L)"));     
do.call(`:`, argv);     
}, o=expected);     

