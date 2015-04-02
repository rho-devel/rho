expected <- eval(parse(text="1e+07"));              
test(id=0, code={              
argv <- eval(parse(text="list(1e+07)"));              
do.call(`abs`, argv);              
}, o=expected);              

