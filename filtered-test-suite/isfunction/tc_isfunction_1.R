expected <- eval(parse(text="TRUE"));      
test(id=0, code={      
argv <- eval(parse(text="list(function (x, y) {    c(x, y)})"));      
do.call(`is.function`, argv);      
}, o=expected);      

