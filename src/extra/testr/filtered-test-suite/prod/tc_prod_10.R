expected <- eval(parse(text="1"));          
test(id=0, code={          
argv <- list();          
do.call(`prod`, argv);          
}, o=expected);          

