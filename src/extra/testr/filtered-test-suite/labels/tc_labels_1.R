expected <- c("(Intercept)", "gravity", "ph", "osmo", "conduct", "urea", 
"log(calc)")
test(id=0, code={
argv <- structure(list(object = structure(c(-469.098459411633, 469.356672501203, 
-0.429918004252249, 0.00364370239091614, -0.256875513692359, 
-0.0204799335117722, 2.00613934942808), .Names = c("(Intercept)", 
"gravity", "ph", "osmo", "conduct", "urea", "log(calc)"))), .Names = "object")
do.call('labels', argv);
},  o = expected);

