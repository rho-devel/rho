expected <- eval(parse(text="-5L"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(list(c(101, 32741, 2147483621, 1.70141183460469e+38, 8.98846567431158e+307)), row.names = c(NA, -5L), class = \"data.frame\"), 1L)"));                 
.Internal(shortRowNames(argv[[1]], argv[[2]]));                 
}, o=expected);                 

