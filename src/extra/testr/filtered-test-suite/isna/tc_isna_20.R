expected <- eval(parse(text="structure(c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE), .Dim = c(12L, 1L), .Dimnames = list(c(\"407\", \"408\", \"409\", \"410\", \"411\", \"412\", \"413\", \"414\", \"415\", \"416\", \"417\", \"418\"), \"conc\"))"));                
test(id=0, code={                
argv <- eval(parse(text="list(structure(list(conc = c(NA, 1.4, NA, NA, NA, NA, NA, NA, 2.2, NA, NA, 0.6)), .Names = \"conc\", row.names = 407:418, class = \"data.frame\"))"));                
do.call(`is.na`, argv);                
}, o=expected);                

