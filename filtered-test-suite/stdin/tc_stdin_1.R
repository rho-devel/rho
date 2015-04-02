expected <- eval(parse(text="structure(0L, class = c(\"terminal\", \"connection\"))"));   
test(id=0, code={   
.Internal(`stdin`());   
}, o=expected);   

