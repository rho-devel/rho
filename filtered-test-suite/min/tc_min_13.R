expected <- eval(parse(text="Inf"));             
test(id=0, code={             
argv <- list();             
do.call(`min`, argv);             
}, o=expected);             

