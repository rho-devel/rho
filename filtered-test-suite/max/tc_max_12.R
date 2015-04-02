expected <- eval(parse(text="4"));      
test(id=0, code={      
argv <- eval(parse(text="list(4L, numeric(0))"));      
do.call(`max`, argv);      
}, o=expected);      

