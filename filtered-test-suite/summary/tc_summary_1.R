expected <- structure(list(n.vars = 2L, n.cases = 88L, statistic = 1.41890959282264, 
    parameter = 15, approx.ok = FALSE, p.value = 0.999997088517426, 
    call = NULL), .Names = c("n.vars", "n.cases", "statistic", 
"parameter", "approx.ok", "p.value", "call"), class = "summary.table")
test(id=16, code={
argv <- structure(list(object = structure(c(4L, 4L, 4L, 4L, 4L, 3L, 4L, 
4L, 4L, 4L, 3L, 4L, 3L, 4L, 4L, 4L, 4L, 2L, 4L, 3L, 4L, 4L, 4L, 
2L), .Dim = c(6L, 4L), .Dimnames = structure(list(c("25-34", 
"35-44", "45-54", "55-64", "65-74", "75+"), c("0-39g/day", "40-79", 
"80-119", "120+")), .Names = c("", "")), class = "table")), .Names = "object")
do.call('summary', argv);
},  o = expected);

