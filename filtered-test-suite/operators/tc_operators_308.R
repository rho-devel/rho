expected <- eval(parse(text="structure(508L, class = \"octmode\")"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(120L, class = \"octmode\"), \"644\")"));      
do.call(`|`, argv);      
}, o=expected);      

