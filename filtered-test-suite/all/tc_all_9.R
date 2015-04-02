expected <- eval(parse(text="TRUE"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(1L, .Names = \"show\"))"));     
do.call(`all`, argv);     
}, o=expected);     

