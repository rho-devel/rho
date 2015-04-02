expected <- eval(parse(text="2819"));      
test(id=0, code={      
argv <- eval(parse(text="list(2819.50000004)"));      
do.call(`trunc`, argv);      
}, o=expected);      

