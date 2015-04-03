expected <- eval(parse(text="TRUE"));      
test(id=0, code={      
argv <- eval(parse(text="list(\"./myTst2/man/DocLink-class.Rd\")"));      
do.call(`nzchar`, argv);      
}, o=expected);      

