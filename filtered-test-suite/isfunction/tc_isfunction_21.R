expected <- TRUE           
test(id=134, code={           
argv <- list(function (x, i, ...)            
structure(NextMethod("["), class = class(x)))           
do.call('is.function', argv);           
},  o = expected);           
           
