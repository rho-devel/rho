expected <- eval(parse(text="TRUE"));          
test(id=0, code={          
argv <- eval(parse(text="list(structure(list(2L), class = \"numeric_version\"), \"2\")"));          
do.call(`>=`, argv);          
}, o=expected);          

