expected <- eval(parse(text="1260L"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(c(6L, 12L, 18L, 24L, 30L, 36L, 42L, 48L, 54L, 60L, 66L, 72L, 78L, 84L, 90L, 96L, 102L, 108L, 114L, 120L), .Dim = 4:5, .Dimnames = list(NULL, c(\"V5\", \"V6\", \"V7\", \"V8\", \"V9\"))))"));       
do.call(`sum`, argv);       
}, o=expected);       

