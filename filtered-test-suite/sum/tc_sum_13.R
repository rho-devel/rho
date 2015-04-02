expected <- eval(parse(text="0"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(numeric(0))"));                 
do.call(`sum`, argv);                 
}, o=expected);                 

