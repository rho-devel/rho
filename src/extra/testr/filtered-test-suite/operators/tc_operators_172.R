expected <- eval(parse(text="1008536447L"));              
test(id=0, code={              
argv <- eval(parse(text="list(-65205377L, 1073741824L)"));              
do.call(`%%`, argv);              
}, o=expected);              

