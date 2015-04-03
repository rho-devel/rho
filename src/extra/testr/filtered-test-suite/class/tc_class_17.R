expected <- eval(parse(text="\"numeric\""));              
test(id=0, code={              
argv <- eval(parse(text="list(c(-10, -10, -10, NA, NA, 150, 170, 180, NA, NA, 310, 330, 340, 350, 370, 380))"));              
do.call(`class`, argv);              
}, o=expected);              

