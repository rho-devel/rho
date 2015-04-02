expected <- eval(parse(text="FALSE"));            
test(id=0, code={            
argv <- eval(parse(text="list(structure(list(x = c(0.3, 3.6, 6.2, 3.8, 3.1, 4.1, 6), y = c(6.1, 6.2, 5.2, 2.3, 1.1, 0.8, 0.1)), .Names = c(\"x\", \"y\"), row.names = c(1L, 4L, 12L, 31L, 37L, 48L, 50L), class = \"data.frame\"))"));            
do.call(`is.language`, argv);            
}, o=expected);            

