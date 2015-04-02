expected <- TRUE           
test(id=133, code={           
argv <- list(function (cpu = Inf, elapsed = Inf, transient = FALSE)            
.Internal(setTimeLimit(cpu, elapsed, transient)))           
do.call('is.function', argv);           
},  o = expected);           
           
