expected <- TRUE           
test(id=76, code={           
argv <- structure(list(x = structure(list(a = NA, b = NA_integer_, c = NA_real_,            
    d = NA_complex_, e = 1, f = 1L, g = 1:3, h = c(NA, 1L, 2L,            
    3L), i = NA_character_, j = c("foo", NA, "bar")), .Names = c("a",            
"b", "c", "d", "e", "f", "g", "h", "i", "j")), y = structure(list(           
    a = NA, b = NA_integer_, c = NA_real_, d = NA_complex_, e = 1,            
    f = 1L, g = 1:3, h = c(NA, 1L, 2L, 3L), i = NA_character_,            
    j = c("foo", NA, "bar")), .Names = c("a", "b", "c", "d",            
"e", "f", "g", "h", "i", "j"))), .Names = c("x", "y"))           
do.call('identical', argv);           
},  o = expected);           
           
