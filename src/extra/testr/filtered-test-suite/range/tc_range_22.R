expected <- eval(parse(text="1:2"));           
test(id=0, code={           
argv <- eval(parse(text="list(structure(c(1L, 1L, 2L, 1L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 2L, 1L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 1L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 1L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 2L, 1L, 2L, 1L, 1L, 1L, 1L, 2L, 1L, 1L, 2L, 2L, 1L, 2L, 1L, NA, 1L, 1L, 2L, 1L, 1L, NA, 2L, 2L, 2L, 1L, 1L, 2L, 2L, 2L, 1L, 2L, 1L, NA, 2L, 2L, 1L, NA, 2L, 2L, NA, 1L, 2L, 1L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 1L, 1L, 2L, 2L, 1L, 2L, 1L), .Dim = c(20L, 6L), .Dimnames = list(c(\"ant\", \"bee\", \"cat\", \"cpl\", \"chi\", \"cow\", \"duc\", \"eag\", \"ele\", \"fly\", \"fro\", \"her\", \"lio\", \"liz\", \"lob\", \"man\", \"rab\", \"sal\", \"spi\", \"wha\"), c(\"war\", \"fly\", \"ver\", \"end\", \"gro\", \"hai\"))), na.rm = TRUE)"));           
do.call(`range`, argv);           
}, o=expected);           

