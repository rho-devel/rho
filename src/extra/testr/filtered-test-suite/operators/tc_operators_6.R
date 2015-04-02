expected <- eval(parse(text="structure(-93573.0193693447, .Names = \"x\")"));                
test(id=0, code={                
argv <- eval(parse(text="list(structure(5.34872109992236, .Names = \"x\"), structure(-17494.4659893938, .Names = \"x\"))"));                
do.call(`*`, argv);                
}, o=expected);                

