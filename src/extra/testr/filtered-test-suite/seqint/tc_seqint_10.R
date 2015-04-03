expected <- eval(parse(text="c(0L, -1L)"));            
test(id=0, code={            
argv <- eval(parse(text="list(from = 0, to = structure(-1, .Names = \"c0\"))"));            
do.call(`seq.int`, argv);            
}, o=expected);            

