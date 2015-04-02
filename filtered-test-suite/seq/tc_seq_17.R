expected <- eval(parse(text="1L"));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(list(structure(\" A Simple Plot and Legend Demo \", Rd_tag = \"TEXT\")), Rd_tag = \"Rd\", class = \"Rd\"))"));               
do.call(`seq_along`, argv);               
}, o=expected);               

