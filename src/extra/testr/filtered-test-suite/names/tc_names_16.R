expected <- eval(parse(text="character(0)"));                   
test(id=0, code={                   
argv <- eval(parse(text="list(structure(list(), .Names = character(0)))"));                   
do.call(`names`, argv);                   
}, o=expected);                   

