expected <- eval(parse(text="structure(c(7, 14, 21, 28), .Names = c(\"Urban Female\", \"Urban Male\", \"Rural Female\", \"Rural Male\"))"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(c(7, 7, 7, 7), .Dim = 4L, .Dimnames = list(c(\"Urban Female\", \"Urban Male\", \"Rural Female\", \"Rural Male\"))))"));  
do.call(`cumsum`, argv);  
}, o=expected);  

