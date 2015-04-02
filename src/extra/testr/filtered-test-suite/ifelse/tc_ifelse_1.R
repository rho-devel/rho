expected <- c("True", "True", "False", "True", "False")
test(id=1, code={
argv <- structure(list(test = c(TRUE, TRUE, FALSE, TRUE, FALSE), yes = "True", 
    no = "False"), .Names = c("test", "yes", "no"))
do.call('ifelse', argv);
},  o = expected);

