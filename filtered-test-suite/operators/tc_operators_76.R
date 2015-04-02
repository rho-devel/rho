expected <- eval(parse(text="0:1"));             
test(id=0, code={             
argv <- eval(parse(text="list(FALSE, 1)"));             
do.call(`:`, argv);             
}, o=expected);             

