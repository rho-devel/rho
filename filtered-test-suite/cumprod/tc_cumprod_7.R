expected <- eval(parse(text="structure(numeric(0), .Names = character(0))"));          
test(id=0, code={          
argv <- eval(parse(text="list(structure(list(), .Names = character(0)))"));          
do.call(`cumprod`, argv);          
}, o=expected);          

