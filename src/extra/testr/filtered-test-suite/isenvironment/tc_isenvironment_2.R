expected <- eval(parse(text="FALSE"));          
test(id=0, code={          
argv <- eval(parse(text="list(structure(3.14159265358979, class = structure(\"3.14159265358979\", class = \"testit\")))"));          
do.call(`is.environment`, argv);          
}, o=expected);          

