expected <- eval(parse(text="structure(list(x = structure(1:8, .Dim = structure(8L, .Names = \"voice.part\")), 4), .Names = c(\"x\", \"\"))"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(structure(list(x = structure(1:8, .Dim = structure(8L, .Names = \"voice.part\"))), .Names = \"x\"), list(4))"));                  
do.call(`c`, argv);                  
}, o=expected);                  

