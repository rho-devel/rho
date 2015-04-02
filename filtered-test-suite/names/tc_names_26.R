expected <- eval(parse(text="c(\"object\", \"slots\", \"dataPart\", \"class\")"));                   
test(id=0, code={                   
argv <- eval(parse(text="list(structure(list(object = structure(3.14159265358979, comment = \"Start with pi\"), slots = \"comment\", dataPart = TRUE, class = structure(\"classPrototypeDef\", package = \"methods\")), .Names = c(\"object\", \"slots\", \"dataPart\", \"class\")))"));                   
do.call(`names`, argv);                   
}, o=expected);                   

