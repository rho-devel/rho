expected <- structure(list(statistic = structure(0.87901108669074, .Names = "t"), 
    parameter = structure(19, .Names = "df"), p.value = 0.390376937081292, 
    conf.int = structure(c(-0.332667989442433, 0.814407243771461
    ), conf.level = 0.95), estimate = structure(0.240869627164514, .Names = "mean of x"), 
    null.value = structure(0, .Names = "mean"), alternative = "two.sided", 
    method = "One Sample t-test", data.name = "x"), .Names = c("statistic", 
"parameter", "p.value", "conf.int", "estimate", "null.value", 
"alternative", "method", "data.name"), class = "htest")
test(id=81, code={
argv <- structure(list(x = structure(list(statistic = structure(0.87901108669074, .Names = "t"), 
    parameter = structure(19, .Names = "df"), p.value = 0.390376937081292, 
    conf.int = structure(c(-0.332667989442433, 0.814407243771461
    ), conf.level = 0.95), estimate = structure(0.240869627164514, .Names = "mean of x"), 
    null.value = structure(0, .Names = "mean"), alternative = "two.sided", 
    method = "One Sample t-test", data.name = "x"), .Names = c("statistic", 
"parameter", "p.value", "conf.int", "estimate", "null.value", 
"alternative", "method", "data.name"), class = "htest")), .Names = "x")
do.call('print', argv);
},  o = expected);

