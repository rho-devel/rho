expected <- eval(parse(text="FALSE"));             
test(id=0, code={             
argv <- list();             
do.call(`any`, argv);             
}, o=expected);             

