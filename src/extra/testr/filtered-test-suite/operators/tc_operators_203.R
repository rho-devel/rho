expected <- eval(parse(text="structure(character(0), .Dim = c(0L, 7L), .Dimnames = list(NULL, c(\"description\", \"class\", \"mode\", \"text\", \"isopen\", \"can read\", \"can write\")))"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(character(0), .Dim = c(0L, 7L), .Dimnames = list(NULL, c(\"description\", \"class\", \"mode\", \"text\", \"isopen\", \"can read\", \"can write\"))))"));     
do.call(`(`, argv);     
}, o=expected);     

