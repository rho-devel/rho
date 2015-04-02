expected <- structure(1:3, id = "An Example")     
test(id=2, code={     
argv <- structure(list(x = structure(1:3, id = "An Example", class = structure("numWithId", package = ".GlobalEnv"))), .Names = "x")     
do.call('xtfrm.default', argv);     
},  o = expected);     
     
