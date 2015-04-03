expected <- eval(parse(text="structure(character(0), .Names = character(0))"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(character(0), .Names = character(0)))"));   
do.call(`enc2native`, argv);   
}, o=expected);   

