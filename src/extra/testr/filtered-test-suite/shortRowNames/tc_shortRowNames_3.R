expected <- eval(parse(text="1:2"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(list(weight = c(4.17, 5.58), group = structure(c(1L, 1L), .Label = c(\"Ctl\", \"Trt\"), class = \"factor\")), .Names = c(\"weight\", \"group\"), row.names = 1:2, class = \"data.frame\"), 0L)"));       
.Internal(`shortRowNames`(argv[[1]], argv[[2]]));       
}, o=expected);       

