expected <- eval(parse(text="NA_real_"));          
test(id=0, code={          
argv <- eval(parse(text="list(NA_integer_)"));          
do.call(`prod`, argv);          
}, o=expected);          

