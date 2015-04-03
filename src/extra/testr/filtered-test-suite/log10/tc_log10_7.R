expected <- eval(parse(text="structure(numeric(0), .Dim = c(20L, 0L), .Dimnames = list(c(\"ant\", \"bee\", \"cat\", \"cpl\", \"chi\", \"cow\", \"duc\", \"eag\", \"ele\", \"fly\", \"fro\", \"her\", \"lio\", \"liz\", \"lob\", \"man\", \"rab\", \"sal\", \"spi\", \"wha\"), NULL))"));         
test(id=0, code={         
argv <- eval(parse(text="list(structure(numeric(0), .Dim = c(20L, 0L), .Dimnames = list(c(\"ant\", \"bee\", \"cat\", \"cpl\", \"chi\", \"cow\", \"duc\", \"eag\", \"ele\", \"fly\", \"fro\", \"her\", \"lio\", \"liz\", \"lob\", \"man\", \"rab\", \"sal\", \"spi\", \"wha\"), NULL)))"));         
do.call(`log10`, argv);         
}, o=expected);         

