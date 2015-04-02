expected <- eval(parse(text="structure(c(8.40000009536743, 8.80000019073486, 14, 14, 22, 22), .Names = c(\"sec1\", \"sec2\", \"min1\", \"min2\", \"hour1\", \"hour2\"))"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(list(sec = c(8.40000009536743, 8.80000019073486), min = c(14L, 14L), hour = c(22L, 22L)), .Names = c(\"sec\", \"min\", \"hour\")), TRUE, TRUE)"));       
.Internal(`unlist`(argv[[1]], argv[[2]], argv[[3]]));       
}, o=expected);       

