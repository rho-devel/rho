expected <- NULL     
test(id=0, code={     
argv <- structure(list(expr = expression(quote(x <- c(1, x)))), .Names = "expr")     
do.call('.doTrace', argv);     
},  o = expected);     
     
