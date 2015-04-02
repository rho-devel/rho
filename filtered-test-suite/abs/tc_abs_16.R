expected <- eval(parse(text="3.31827701955945e-05"));      
test(id=0, code={      
argv <- eval(parse(text="list(-3.31827701955945e-05)"));      
do.call(`abs`, argv);      
}, o=expected);      

