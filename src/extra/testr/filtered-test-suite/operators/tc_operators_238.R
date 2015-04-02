expected <- eval(parse(text="-5"));      
test(id=0, code={      
argv <- eval(parse(text="list(5)"));      
do.call(`-`, argv);      
}, o=expected);      

