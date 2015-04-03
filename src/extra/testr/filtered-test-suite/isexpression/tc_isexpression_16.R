expected <- eval(parse(text="FALSE"));              
test(id=0, code={              
argv <- eval(parse(text="list(structure(c(\"***\", \"***\", \"*\", \"*\"), legend = \"0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1\", class = \"noquote\"))"));              
do.call(`is.expression`, argv);              
}, o=expected);              

