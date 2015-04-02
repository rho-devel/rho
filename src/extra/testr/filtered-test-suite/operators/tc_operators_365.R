expected <- eval(parse(text="c(1.6768574855882, 1.6768574855882, 1.6768574855882, 1.6768574855882, 1.6768574855882, 0, 0, 0, 0, 0, 0, 0, 0, NA)"));                
test(id=0, code={                
argv <- eval(parse(text="list(structure(1.6768574855882, .Names = \"x\"), c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, NA))"));                
do.call(`*`, argv);                
}, o=expected);                

