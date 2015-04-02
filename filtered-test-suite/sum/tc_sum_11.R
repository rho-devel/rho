expected <- eval(parse(text="56+1i"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(1:10, 1+1i)"));                 
do.call(`sum`, argv);                 
}, o=expected);                 

