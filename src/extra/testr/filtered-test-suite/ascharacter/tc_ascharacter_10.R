expected <- eval(parse(text="character(0)"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(character(0), package = character(0), class = structure(\"ObjectsWithPackage\", package = \"methods\")))"));        
do.call(`as.character`, argv);        
}, o=expected);        

