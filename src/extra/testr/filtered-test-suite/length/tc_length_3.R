expected <- eval(parse(text="1L"));          
test(id=0, code={          
argv <- eval(parse(text="list(structure(\"     \\\"Le français, c'est façile: Règles, Liberté, Egalité, Fraternité...\\\")\\n\", Rd_tag = \"RCODE\"))"));          
do.call(`length`, argv);          
}, o=expected);          

