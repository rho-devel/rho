expected <- eval(parse(text="FALSE"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(16146, class = \"Date\"))"));      
do.call(`is.numeric`, argv);      
}, o=expected);      

