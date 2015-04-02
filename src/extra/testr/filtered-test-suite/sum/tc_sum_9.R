expected <- eval(parse(text="0L"));                 
test(id=0, code={                 
argv <- list();                 
do.call(`sum`, argv);                 
}, o=expected);                 

