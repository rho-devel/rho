expected <- structure(c(15, 37, 30, 18, 8, 20, 42.7, 29.3), .Dim = c(4L, 
2L), .Dimnames = structure(list(Evaluation = c("very good", "good", 
"bad", "very bad"), Location = c("city centre", "suburbs")), .Names = c("Evaluation", 
"Location")))
test(id=0, code={
argv <- structure(list(x = structure(c(15, 37, 30, 18, 8, 20, 42.7, 29.3
), .Dim = c(4L, 2L), .Dimnames = structure(list(Evaluation = c("very good", 
"good", "bad", "very bad"), Location = c("city centre", "suburbs"
)), .Names = c("Evaluation", "Location")))), .Names = "x")
do.call('unique', argv);
},  o = expected);

