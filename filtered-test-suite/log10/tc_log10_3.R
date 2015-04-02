expected <- eval(parse(text="structure(-4.09982455997299, .Names = \"value\")"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(7.94649180820227e-05, .Names = \"value\"))"));   
do.call(`log10`, argv);   
}, o=expected);   

