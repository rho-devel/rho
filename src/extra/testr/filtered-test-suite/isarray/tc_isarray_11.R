expected <- eval(parse(text="FALSE"));            
test(id=0, code={            
argv <- eval(parse(text="list(structure(list(c0 = logical(0)), .Names = \"c0\", row.names = integer(0), class = \"difftime\"))"));            
do.call(`is.array`, argv);            
}, o=expected);            

