expected <- eval(parse(text="c(6L, 1L)"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(structure(list(day = structure(1:6, .Label = c(\"2012-06-01\", \"2012-06-02\", \"2012-06-03\", \"2012-06-04\", \"2012-06-05\", \"2012-06-06\", \"2012-06-07\"), class = \"factor\")), .Names = \"day\", row.names = c(1L, 5L, 9L, 13L, 17L, 21L), class = \"data.frame\"))"));                  
do.call(`dim`, argv);                  
}, o=expected);                  

