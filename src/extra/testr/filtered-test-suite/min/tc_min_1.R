expected <- eval(parse(text="1L"));     
test(id=0, code={     
argv <- eval(parse(text="list(c(10L, 1L))"));     
do.call(`min`, argv);     
}, o=expected);     

