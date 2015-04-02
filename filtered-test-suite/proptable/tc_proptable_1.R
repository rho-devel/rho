expected <- structure(c(0.15, 0.37, 0.3, 0.18, 0.08, 0.2, 0.426666666666667, 
0.293333333333333), .Dim = c(4L, 2L), .Dimnames = structure(list(
    Evaluation = c("very good", "good", "bad", "very bad"), Location = c("city centre", 
    "suburbs")), .Names = c("Evaluation", "Location")))
test(id=0, code={
argv <- structure(list(x = structure(c(15L, 37L, 30L, 18L, 12L, 30L, 
64L, 44L), .Dim = c(4L, 2L), .Dimnames = structure(list(Evaluation = c("very good", 
"good", "bad", "very bad"), Location = c("city centre", "suburbs"
)), .Names = c("Evaluation", "Location"))), margin = 2), .Names = c("x", 
"margin"))
do.call('prop.table', argv);
},  o = expected);

