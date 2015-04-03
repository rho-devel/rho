expected <- eval(parse(text="1"));          
test(id=0, code={          
argv <- eval(parse(text="list(numeric(0))"));          
do.call(`prod`, argv);          
}, o=expected);          

