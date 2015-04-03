expected <- eval(parse(text="0:1000000"));             
test(id=0, code={             
argv <- eval(parse(text="list(0, 1000000L)"));             
do.call(`:`, argv);             
}, o=expected);             

