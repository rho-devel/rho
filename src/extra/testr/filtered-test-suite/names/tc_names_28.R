expected <- eval(parse(text="\"zz\""));                   
test(id=0, code={                   
argv <- eval(parse(text="list(structure(list(zz = complex(0)), .Names = \"zz\"))"));                   
do.call(`names`, argv);                   
}, o=expected);                   

