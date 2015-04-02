expected <- eval(parse(text="structure(list(), .Names = character(0))"));                
test(id=0, code={                
argv <- eval(parse(text="list(structure(list(war = c(1, 1, 0, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0), fly = c(1, 0, 1, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), ver = c(1, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0), end = c(1, 1, 1, 1, 0, 1, 1, 0, 0, 1, 0, 1, NA, 1, 1, 0, 1, 1, NA, 0), gro = c(0, 0, 1, 1, 0, 0, 0, 1, 0, 1, NA, 0, 0, 1, NA, 0, 0, NA, 1, 0), hai = c(1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0, 1, 0, 1)), .Names = c(\"war\", \"fly\", \"ver\", \"end\", \"gro\", \"hai\"), row.names = c(\"ant\", \"bee\", \"cat\", \"cpl\", \"chi\", \"cow\", \"duc\", \"eag\", \"ele\", \"fly\", \"fro\", \"her\", \"lio\", \"liz\", \"lob\", \"man\", \"rab\", \"sal\", \"spi\", \"wha\"), class = \"data.frame\"), NULL)"));                
do.call(`.subset`, argv);                
}, o=expected);                

