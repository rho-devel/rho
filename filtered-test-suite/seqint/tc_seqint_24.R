expected <- eval(parse(text="c(0, 43200, 86400, 129600, 172800, 216000, 259200, 302400, 345600)"));   
test(id=0, code={   
argv <- eval(parse(text="list(0, structure(345600, tzone = \"GMT\"), 43200)"));   
do.call(`seq.int`, argv);   
}, o=expected);   

