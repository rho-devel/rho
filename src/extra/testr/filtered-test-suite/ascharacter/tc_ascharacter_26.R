expected <- eval(parse(text="\"FALSE\""));                 
test(id=0, code={                 
argv <- eval(parse(text="list(FALSE, useSource = TRUE)"));                 
do.call(`as.character`, argv);                 
}, o=expected);                 

