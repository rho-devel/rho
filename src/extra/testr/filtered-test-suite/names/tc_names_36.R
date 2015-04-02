expected <- eval(parse(text="\"head\""));         
test(id=0, code={         
argv <- eval(parse(text="list(structure(list(head = logical(0)), .Names = \"head\", class = \"data.frame\", row.names = integer(0)))"));         
do.call(`names`, argv);         
}, o=expected);         

