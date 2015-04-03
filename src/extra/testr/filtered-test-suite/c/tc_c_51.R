expected <- eval(parse(text="structure(list(\"1: In matrix(1:7, 3, 4) :\\n  data length [7] is not a sub-multiple or multiple of the number of rows [3]\", fill = TRUE), .Names = c(\"\", \"fill\"))"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(list(\"1: In matrix(1:7, 3, 4) :\\n  data length [7] is not a sub-multiple or multiple of the number of rows [3]\"), list(), fill = TRUE)"));                  
do.call(`c`, argv);                  
}, o=expected);                  

