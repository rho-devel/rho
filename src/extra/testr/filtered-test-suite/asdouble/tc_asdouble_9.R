expected <- eval(parse(text="c(-0.1, 2.7, NA)"));               
test(id=0, code={               
argv <- eval(parse(text="list(c(\"-.1\", \" 2.7 \", \"B\"))"));               
do.call(`as.double`, argv);               
}, o=expected);               

