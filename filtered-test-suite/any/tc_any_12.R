expected <- eval(parse(text="FALSE"));      
test(id=0, code={      
argv <- eval(parse(text="list(logical(0))"));      
do.call(`any`, argv);      
}, o=expected);      

