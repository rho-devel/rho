expected <- eval(parse(text="\"numeric\""));              
test(id=0, code={              
argv <- eval(parse(text="list(c(325, 257, 303, 315, 380, 153, 263, 242, 206, 344, 258))"));              
do.call(`class`, argv);              
}, o=expected);              

