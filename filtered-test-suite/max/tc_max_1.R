expected <- eval(parse(text="10L"));      
test(id=0, code={      
argv <- eval(parse(text="list(10L, 1L)"));      
do.call(`max`, argv);      
}, o=expected);      

