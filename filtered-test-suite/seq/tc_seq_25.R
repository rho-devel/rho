expected <- eval(parse(text="1L"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(list(.Data = \"numeric\"), .Names = \".Data\"))"));        
do.call(`seq_along`, argv);        
}, o=expected);        

